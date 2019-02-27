# README---------------
# Estimate total open water and accessible open water in all land cover types 
# on each day of the nonbreeding season from updated flooding curves and 
# original CVJV depth curves (i.e., proportion of open water in each land cover 
# type that is of suitable depth for shorebird foraging habitat), plus data 
# from Bird Returns and WHEP programs
#
# PACKAGES
library(tidyverse)
library(bioenergmod) # devtools::install_github("kdybala/bioenergmod")

# INPUTS
annual_acres <- 'data/landcover_totals_cvjv.csv'
floodcurves <- 'output/open_water_annual.csv'
depthcurves <- 'data/cvjv_orig/depth_curves.csv'
br_ts <- 'data/BR_timeseries.csv'
whep_ts <- 'data/WHEP_timeseries.csv'

# OUTPUTS
habitat_change <- 'output/habitat_change.RData'


# CALCULATE HABITAT AVAILABILITY--------

base <- read_csv(here::here(annual_acres), col_types = cols()) %>% 
  filter(!(habitat %in% c('seas', 'perm'))) %>%
  filter(year != 2017) %>%
  split(.$year)

flood <- read_csv(here::here(floodcurves)) %>%
  mutate(prop.perm = as.numeric(prop.perm)) %>%
  split(.$group)

depth <- read_csv(here::here(depthcurves)) %>%
  filter(habitat != 'corn') %>%
  mutate(habitat = recode(habitat, corn_north = 'corn'))


# calculate habitat availability in each year, with varying base acres and
#   flood curves (excluding incentive acres)
change <- pmap(list(base, flood), 
               calculate_habitat_change,
               time = 'yday', value = 'fit',
               accessible = depth, wetsplit = TRUE)
names(change) = names(flood)


# now append incentive programs (by year group, change variable, and day of year):
incentives <- bind_rows(read_csv(here::here(br_ts), col_types = cols()), 
                        read_csv(here::here(whep_ts), col_types = cols())) %>%
  rename(openwater = available, time = yday) %>%
  gather(openwater:prop.accessible, key = 'var', value = 'value') %>%
  mutate(var = factor(var, 
                      levels = c('openwater', 'added', 'returned', 'accessible', 'prop.accessible'))) %>%
  spread(key = 'habitat', value = 'value') %>%
  arrange(group, var, time) %>%
  split(.$group) 

change_all <- pmap(list(change, incentives),
     function(a, b) {
       pmap(list(a, b %>% split(.$var) %>% map(~ .x %>% select(-var, -group))),
            full_join, by = 'time')
     })

save(change_all, file = here::here(habitat_change))

# --> Necessary? already marked as not suitable in the depth curves
# ## assume rice, corn, and other crops are not accessible prior to 1 September (day 63)
# change$added[1:62,c('other','corn_north','rice')] = 0
# change$returned[1:62,c('corn_north','other','rice')] = 0

# HABITAT AVAILABILITY STATS-----------------

# peak open water by year
change_all %>% 
  map_dfr(~ .x[['openwater']] %>% 
        gather(-time, key = 'habitat', value = 'area') %>%
        group_by(time) %>%
        summarize(area = sum(area)) %>% 
        ungroup() %>%
        summarize(time = time[area == max(area)],
                  area = max(area)),
        .id = 'group')
# ranges days 180-200; 198545 - 291430 ha

# peak accessible by year
change_all %>% 
  map_dfr(~ .x[['accessible']] %>% 
            gather(-time, key = 'habitat', value = 'area') %>%
            group_by(time) %>%
            summarize(area = sum(area)) %>% 
            ungroup() %>%
            summarize(time = time[area == max(area)],
                      area = max(area)),
          .id = 'group')
# ranges days 217-229; 84290 - 118732 ha

# peak accessible without incentive programs:
change_all %>% 
  map_dfr(~ .x[['accessible']] %>% 
            select(-br, -whep_vardd, -whep_fall) %>%
            gather(-time, key = 'habitat', value = 'area') %>%
            group_by(time) %>%
            summarize(area = sum(area)) %>% 
            ungroup() %>%
            summarize(time = time[area == max(area)],
                      area = max(area)),
          .id = 'group')
# ranges days 213-236; 71167 - 105394

# PLOTS-------------------
pal = scales::viridis_pal()(8)
ymax = 310
scale = 1000
ylab = 'total open water (ha, thousands)'
ylab2 = 'accessible habitat (ha, thousands)'
theme = theme(legend.position = c(0.01,1))
theme2 = theme(legend.position = 'none')
scalex = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                            labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                       'Feb', 'Mar', 'Apr', 'May'))
scalex2 = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                            labels = rep('', 11))

# open water: (order variables in stack from top to bottom)

a <- plot_bioenergmod(change_all$`2013-14`$openwater %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme


b <- plot_bioenergmod(change_all$`2014-15`$openwater %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme2

c <- plot_bioenergmod(change_all$`2015-16`$openwater %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

d <- plot_bioenergmod(change_all$`2016-17`$openwater %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

cowplot::plot_grid(a, b, c, d)



# accessible habitat:

e <- plot_bioenergmod(change_all$`2013-14`$accessible %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab2, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme

f <- plot_bioenergmod(change_all$`2014-15`$accessible %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme2

g <- plot_bioenergmod(change_all$`2015-16`$accessible %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab2, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

h <- plot_bioenergmod(change_all$`2016-17`$accessible %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

cowplot::plot_grid(e, f, g, h)


plot_bioenergmod(change_all$`2013-14`$added %>% 
                   select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                          seas, perm), 
                 ylab = ylab2, scale = scale, palette = pal) +
  scalex2 + theme + xlim(2, 319)
                         
plot_bioenergmod(change_all$`2013-14`$returned %>% 
                   select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                          seas, perm), 
                 ylab = ylab2, scale = scale, palette = pal) +
  scalex2 + theme + xlim(2, 319)
