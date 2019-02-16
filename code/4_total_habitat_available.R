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
habitat_available <- 'output/habitat_available.csv'
habitat_accessible <- 'output/habitat_accessible.csv'
habitat_added <- 'output/habitat_added.csv'
habitat_returned <- 'output/habitat_returned.csv'
habitat_prop.accessible <- 'output/habitat_prop.accessible.csv'

# CALCULATE HABITAT AVAILABILITY--------

base <- read_csv(here::here(annual_acres)) %>% 
  filter(!(habitat %in% c('seas', 'perm')))

flood <- read_csv(here::here(floodcurves)) %>%
  mutate(prop.perm = as.numeric(prop.perm))

depth <- read_csv(here::here(depthcurves)) %>%
  filter(habitat != 'corn') %>%
  mutate(habitat = recode(habitat, corn_north = 'corn'))


change13 <- calculate_habitat_change(tothabitat = base %>% filter(year == 2013), 
                                     flood = flood %>% filter(group == '2013-14'), 
                                     time = 'yday', value = 'fit',
                                     accessible = depth, wetsplit = TRUE)
change14 <- calculate_habitat_change(tothabitat = base %>% filter(year == 2014), 
                                     flood = flood %>% filter(group == '2014-15'), 
                                     time = 'yday', value = 'fit',
                                     accessible = depth, wetsplit = TRUE)
change15 <- calculate_habitat_change(tothabitat = base %>% filter(year == 2015), 
                                     flood = flood %>% filter(group == '2015-16'), 
                                     time = 'yday', value = 'fit',
                                     accessible = depth, wetsplit = TRUE)
change16 <- calculate_habitat_change(tothabitat = base %>% filter(year == 2016), 
                                     flood = flood %>% filter(group == '2016-17'), 
                                     time = 'yday', value = 'fit',
                                     accessible = depth, wetsplit = TRUE)

# COMPILE DATA ACROSS YEARS---------
# and append incentive programs
br <- read_csv(here::here(br_ts))
whep <- read_csv(here::here(whep_ts))

available <- bind_rows(change13$openwater %>% mutate(group = '2013-14'),
                       change14$openwater %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$openwater %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$openwater %>% mutate(group = '2016-17')) %>%
  full_join(br %>% select(group, time = yday, br = available), 
            by = c('time', 'group')) %>%
  full_join(whep %>% select(habitat, group, time = yday, available) %>% 
              spread(key = habitat, value = available),
            by = c('time', 'group'))

accessible <- bind_rows(change13$accessible %>% mutate(group = '2013-14'),
                        change14$accessible %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$accessible %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$accessible %>% mutate(group = '2016-17')) %>%
  full_join(br %>% select(group, time = yday, br = accessible), 
            by = c('time', 'group')) %>%
  full_join(whep %>% select(habitat, group, time = yday, available) %>% 
              spread(key = habitat, value = available),
            by = c('time', 'group'))

added <- bind_rows(change13$added %>% mutate(group = '2013-14'),
                   change14$added %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$added %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$added %>% mutate(group = '2016-17')) %>%
  full_join(br %>% select(group, time = yday, br = added), 
            by = c('time', 'group')) %>%
  full_join(whep %>% select(habitat, group, time = yday, added) %>% 
              spread(key = habitat, value = added),
            by = c('time', 'group'))

returned <- bind_rows(change13$returned %>% mutate(group = '2013-14'),
                      change14$returned %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$returned %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$returned %>% mutate(group = '2016-17')) %>%
  full_join(br %>% select(group, time = yday, br = returned), 
            by = c('time', 'group')) %>%
  full_join(whep %>% select(habitat, group, time = yday, returned) %>% 
              spread(key = habitat, value = returned),
            by = c('time', 'group'))

prop.accessible <- bind_rows(change13$prop.accessible %>% mutate(group = '2013-14'),
                             change14$prop.accessible %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$prop.accessible %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$prop.accessible %>% mutate(group = '2016-17')) %>%
  mutate(br = 1, #assume 100% accessible whenever flooded
         whep_fall = 1,
         whep_vardd = whep %>% filter(habitat == 'whep_vardd') %>% 
           mutate(prop = accessible/available) %>% pull(prop),
         whep_vardd = case_when(is.nan(whep_vardd) ~ 1,
                                TRUE ~ whep_vardd))

write_csv(available, here::here(habitat_available))
write_csv(accessible, here::here(habitat_accessible))
write_csv(added, here::here(habitat_added))
write_csv(returned, here::here(habitat_returned))
write_csv(prop.accessible, here::here(habitat_prop.accessible))

# --> Necessary? already marked as not suitable in the depth curves
# ## assume rice, corn, and other crops are not accessible prior to 1 September (day 63)
# change$added[1:62,c('other','corn_north','rice')] = 0
# change$returned[1:62,c('corn_north','other','rice')] = 0

# HABITAT AVAILABILITY STATS-----------------

# peak open water by year
available %>% 
  gather(corn:seas, br:whep_vardd, key = 'habitat', value = 'area') %>%
  group_by(group, time) %>%
  summarize(area = sum(area)) %>%
  group_by(group) %>%
  summarize(time = time[area == max(area)],
            area = max(area))
# ranges days 181-204; 197936 - 291247 ha

# peak accessible by year
accessible %>% 
  gather(corn:seas, br:whep_vardd, key = 'habitat', value = 'area') %>%
  group_by(group, time) %>%
  summarize(area = sum(area)) %>%
  group_by(group) %>%
  summarize(time = time[area == max(area)],
            area = max(area))
# ranges days 216-229; 90015 - 130772 ha

# peak accessible without incentive programs:
accessible %>% 
  select(-br, -whep_fall, -whep_vardd) %>%
  gather(corn:seas, key = 'habitat', value = 'area') %>%
  group_by(group, time) %>%
  summarize(area = sum(area)) %>%
  group_by(group) %>%
  summarize(time = time[area == max(area)],
            area = max(area))
# ranges days 215-236; 74360 - 107118

# PLOTS-------------------
pal = scales::viridis_pal()(8)
ymax = 310
scale = 1000
ylab = 'total open water (ha, thousands)'
theme = theme(legend.position = c(0.01,1))
theme2 = theme(legend.position = 'none')
scalex = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                            labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                       'Feb', 'Mar', 'Apr', 'May'))
scalex2 = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                            labels = rep('', 11))

# open water: (order variables in stack from top to bottom)
a <- plot_bioenergmod(available %>% filter(group == '2013-14') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme


b <- plot_bioenergmod(available %>% filter(group == '2014-15') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme2

c <- plot_bioenergmod(available %>% filter(group == '2015-16') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

d <- plot_bioenergmod(available %>% filter(group == '2016-17') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

cowplot::plot_grid(a, b, c, d)



# accessible habitat:
ylab2 = 'accessible habitat (ha, thousands)'

e <- plot_bioenergmod(accessible %>% filter(group == '2013-14') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab2, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme

f <- plot_bioenergmod(accessible %>% filter(group == '2014-15') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex2 + theme2

g <- plot_bioenergmod(accessible %>% filter(group == '2015-16') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = ylab2, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

h <- plot_bioenergmod(accessible %>% filter(group == '2016-17') %>% 
                        select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                               seas, perm), 
                      ylab = NULL, ymax = ymax, scale = scale, palette = pal) +
  scalex + theme2

cowplot::plot_grid(e, f, g, h)


plot_bioenergmod(added %>% filter(group == '2013-14') %>% 
                   select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                          seas, perm), 
                 ylab = ylab2, scale = scale, palette = pal) +
  scalex2 + theme + xlim(2, 319)
                         
plot_bioenergmod(returned %>% filter(group == '2013-14') %>% 
                   select(time, br, whep_fall, whep_vardd, other, corn, rice, 
                          seas, perm), 
                 ylab = ylab2, scale = scale, palette = pal) +
  scalex2 + theme + xlim(2, 319)
