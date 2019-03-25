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
habitat_daily <- 'output/habitat_daily_stats.csv'
habitat_stats <- 'output/habitat_peak_stats.csv'


# CALCULATE HABITAT AVAILABILITY--------

base <- read_csv(here::here(annual_acres), col_types = cols()) %>% 
  filter(!(habitat %in% c('seas', 'perm'))) %>%
  filter(year != 2017) %>%
  split(.$year)

flood <- read_csv(here::here(floodcurves), col_types = cols()) %>%
  mutate(prop.perm = as.numeric(prop.perm)) %>%
  split(.$group)

depth <- read_csv(here::here(depthcurves), col_types = cols()) %>%
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

habitat_avail <- bind_rows(
  map_dfr(change_all, ~.x[['openwater']] %>% mutate(watertype = 'open'), .id = 'group'),
  map_dfr(change_all, ~.x[['accessible']] %>% mutate(watertype = 'accessible'), .id = 'group')
)
write_csv(habitat_avail, here::here(habitat_daily))

# --> Necessary? already marked as not suitable in the depth curves
# ## assume rice, corn, and other crops are not accessible prior to 1 September (day 63)
# change$added[1:62,c('other','corn_north','rice')] = 0
# change$returned[1:62,c('corn_north','other','rice')] = 0

# HABITAT AVAILABILITY STATS-----------------

peaks <- habitat_avail %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'area') %>%
  mutate(habtype = case_when(habitat %in% c('br_fall', 'br_spring', 'whep_fall', 'whep_vardd') ~ 'incentive_only',
                             TRUE ~ 'none')) %>%
  group_by(group, time, watertype, habtype) %>%
  summarize(area = sum(area)) %>%
  spread(key = habtype, value = area) %>%
  mutate(all = incentive_only + none) %>%
  ungroup() %>%
  select(-incentive_only) %>%
  gather(none:all, key = incentives, value = area) %>%
  group_by(group, watertype, incentives) %>%
  summarize(time = time[area == max(area)],
            area = max(area)) %>%
  arrange(watertype, incentives, group)

write_csv(peaks, here::here(habitat_stats))

# peak open water ranges days 182-200; 197448 - 292102 ha; lowest in 2015-16 and highest in 2016-17
# peak open water without incentive programs ranges days 182-200; 183076 - 284500 ha
# peak accessible ranges days 216-229; 83975 - 118180 ha; lowest in 2015-16 and highest in 2013-14 (due to incentive programs)
# peak accessible without incentive programs ranges days 212-236; 70766 - 105050; lowest in 2015-16 and highest in 2016-17
