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
plot_habitat <- 'figs/habitat_by_year.png'


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
peak_open <- change_all %>% 
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
peak_accessible <- change_all %>% 
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
pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666',
    '#456d28', #add a few more complementary colors
    '#b74374', 
    '#5e2a84',
    '#d2c921')

ymax = 310
scale = 1000
ylab = 'total open water (ha, thousands)'
ylab2 = 'accessible habitat (ha, thousands)'
theme = theme_classic() + 
  theme(legend.position = c(0,1), legend.justification = c(0,1),
        legend.background = element_rect(fill = "transparent"))
theme2 = theme_classic() + theme(legend.position = 'none')
scalex = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                            labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                       'Feb', 'Mar', 'Apr', 'May'),
                            expand = c(0, 0))
scalex2 = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                            labels = NULL, 
                            expand = c(0, 0))
scaley = scale_y_continuous(limits = c(0, ymax))
scaley2 = scale_y_continuous(labels = NULL, limits = c(0, ymax))

# order variables in stack from top to bottom (and match colors)
levels = c('br', 'whep_fall', 'whep_vardd', 'rice', 'corn', 'other', 'seas', 'perm')
pal = c(pointblue.palette[c(10, 11, 9, 3)], 'yellow', pointblue.palette[c(2, 1, 4)])


a <- change_all$`2013-14`$openwater %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex2 + scaley + theme + labs(x = NULL, y = NULL, title = '2013-14') +
  guides(fill = guide_legend(ncol = 2)) +
  geom_vline(data = peak_open %>% filter(group == '2013-14'), aes(xintercept = time))

b <- change_all$`2014-15`$openwater %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex2 + scaley + theme2 + labs(x = NULL, y = NULL, title = '2014-15') +
  geom_vline(data = peak_open %>% filter(group == '2014-15'), aes(xintercept = time))

c <- change_all$`2015-16`$openwater %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex2 + scaley + theme2 + labs(x = NULL, y = NULL, title = '2015-16') +
  geom_vline(data = peak_open %>% filter(group == '2015-16'), aes(xintercept = time))

d <- change_all$`2016-17`$openwater %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex + scaley + theme2 + labs(x = NULL, y = NULL, title = '2016-17') +
  geom_vline(data = peak_open %>% filter(group == '2016-17'), aes(xintercept = time))

cowplot::plot_grid(a, b, c, d, nrow = 4)



# accessible habitat:
ymax = 125
scaley = scale_y_continuous(limits = c(0, ymax))

e <- change_all$`2013-14`$accessible %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex2 + scaley + theme2 + labs(x = NULL, y = NULL, title = '2013-14') +
  guides(fill = guide_legend(ncol = 2)) +
  geom_vline(data = peak_accessible %>% filter(group == '2013-14'), aes(xintercept = time))

f <- change_all$`2014-15`$accessible %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex2 + scaley + theme2 + labs(x = NULL, y = NULL, title = '2014-15') +
  geom_vline(data = peak_accessible %>% filter(group == '2014-15'), aes(xintercept = time))

g <- change_all$`2015-16`$accessible %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex2 + scaley + theme2 + labs(x = NULL, y = NULL, title = '2015-16') +
  geom_vline(data = peak_accessible %>% filter(group == '2015-16'), aes(xintercept = time))

h <- change_all$`2016-17`$accessible %>% 
  gather(-time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot(aes(time, value/scale)) + geom_area(aes(fill = habitat)) +
  scale_fill_manual(values = pal) +
  scalex + scaley + theme2 + labs(x = NULL, y = NULL, title = '2016-17') +
  geom_vline(data = peak_accessible %>% filter(group == '2016-17'), aes(xintercept = time))


cowplot::plot_grid(a, e, b, f, c, g, d, h, nrow = 4, ncol = 2)
ggsave(here::here(plot_habitat))

