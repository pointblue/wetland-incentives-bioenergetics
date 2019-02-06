# README------------
# Estimate proportion of state-wide annual crop totals that fall within
# CVJV primary focus area and each basin using TNC-generated spatial layer
# representing "consistent" classification of general crop classes

# PACKAGES
library(tidyverse)

# INPUT DATA
agstats <- 'data/NASS_totals_statewide.csv'
agdist <- 'data/cvjv_orig/ag_distribution_basins.csv'

# OUTPUT DATA
results <- 'data/NASS_totals_cvjv.csv'


# ANNUAL BASIN TOTALS------------
# allocate annual crop totals to basins (and therefore the CVJV primary focus area)
# based on average spatial distribution of crops in CA (same spatial distribution
# assumed for CVJV non-breeding shorebirds paper)

dist <- read_csv(here::here(agdist)) %>%
  filter(TREND_CATE %in% c('Grass/Pasture', 'Field Crop', 'Row Crop', 'Alfalfa',
                           'Grains', 'Corn', 'Rice')) %>%
  rename(basin = Primary_Ba,
         crop_class = TREND_CATE,
         prop = perc) %>% # it's actually the proportion
  select(ID, basin, crop_class, prop) %>%
  mutate(crop_class = tolower(crop_class),
         crop_class = gsub(' ', '', crop_class),
         crop_class = recode(crop_class, 'grass/pasture' = 'pasture')) 

dat <- read_csv(here::here(agstats)) %>%
  left_join(dist, by = 'crop_class') %>%
  mutate(ha = acres * prop / 2.47105) %>%
  select(crop_class, ID, basin, year, ha) %>%
  filter(basin != 'Suisun') %>%
  filter(!(crop_class == 'corn' & basin %in% c('San Joaquin', 'Tulare'))) %>%
  mutate(crop_class = recode(crop_class,
                             fieldcrop = 'other',
                             rowcrop = 'other',
                             grains = 'other'))

# summarize for 2007-2014 (compare to CVJV totals) --> very close ("other" is lower now)
dat %>% 
  filter(year <= 2014) %>%
  group_by(crop_class, year) %>%
  summarize(ha = sum(ha)) %>%
  group_by(crop_class) %>%
  summarize(ha = mean(ha))

# summarize total for each crop class in each year
sdat <- dat %>% 
  group_by(crop_class, year) %>%
  summarize(ha = sum(ha)) 

write_csv(sdat, here::here(results))
