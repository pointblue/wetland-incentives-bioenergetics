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
annual_acres <- 'data/NASS_totals_cvjv.csv'
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
  filter(!(landcover %in% c('seas', 'perm'))) %>%
  rename(habitat = landcover, area = ha)

flood <- read_csv(here::here(floodcurves)) %>%
  rename(habitat = landcover) %>%
  mutate(habitat = recode(habitat, wetland = 'wetlands'),
         prop.perm = as.numeric(prop.perm))

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
  full_join(whep %>% select(landcover, group, time = yday, available) %>% 
              spread(key = landcover, value = available),
            by = c('time', 'group'))

accessible <- bind_rows(change13$accessible %>% mutate(group = '2013-14'),
                        change14$accessible %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$accessible %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$accessible %>% mutate(group = '2016-17')) %>%
  full_join(br %>% select(group, time = yday, br = accessible), 
            by = c('time', 'group')) 
## Note: estimates for accessibility in WHEP fields not yet available

added <- bind_rows(change13$added %>% mutate(group = '2013-14'),
                   change14$added %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$added %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$added %>% mutate(group = '2016-17')) %>%
  full_join(br %>% select(group, time = yday, br = added), 
            by = c('time', 'group')) %>%
  full_join(whep %>% select(landcover, group, time = yday, added) %>% 
              spread(key = landcover, value = added),
            by = c('time', 'group'))

returned <- bind_rows(change13$returned %>% mutate(group = '2013-14'),
                      change14$returned %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$returned %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$returned %>% mutate(group = '2016-17')) %>%
  full_join(br %>% select(group, time = yday, br = returned), 
            by = c('time', 'group')) %>%
  full_join(whep %>% select(landcover, group, time = yday, returned) %>% 
              spread(key = landcover, value = returned),
            by = c('time', 'group'))

prop.accessible <- bind_rows(change13$prop.accessible %>% mutate(group = '2013-14'),
                             change14$prop.accessible %>% mutate(group = '2014-15')) %>%
  bind_rows(change15$prop.accessible %>% mutate(group = '2015-16')) %>%
  bind_rows(change16$prop.accessible %>% mutate(group = '2016-17')) %>%
  mutate(br = 1) #assume 100% accessible whenever flooded
## Note: estimates for accessibility in WHEP fields not yet available


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
  gather(corn:rice, br, key = 'habitat', value = 'area') %>%
  group_by(group, time) %>%
  summarize(area = sum(area)) %>%
  group_by(group) %>%
  summarize(time = time[area == max(area)],
            area = max(area))
# ranges days 181-204; 150354 - 242628 ha

# peak accessible
accessible %>% 
  gather(corn:rice, br, key = 'habitat', value = 'area') %>%
  group_by(group, time) %>%
  summarize(area = sum(area)) %>%
  group_by(group) %>%
  summarize(time = time[area == max(area)],
            area = max(area))
# ranges days 216-229; 71318 - 102847 ha
