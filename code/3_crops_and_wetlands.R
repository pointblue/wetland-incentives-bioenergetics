# README---------------
# Compile time series of habitat available and accessible from crops and 
# wetlands not enrolled in incentive programs. Because areas of these programs
# are relatively small, ignore them?
#
# STEP 1: update area of potential habitat in each year through 2017
#

# PACKAGES
# devtools::install_github('emraher/rnass')
library(rnass) 
library(tidyverse)

# API
# acquire from: https://quickstats.nass.usda.gov/api
# --> assume stored as NASS_TOKEN in .Renviron

# FUNCTIONS
# custom function to extract the specific crop types we need for this project
#  for a given year
source(here::here('functions/extract_nass.R'))

# INPUTS
agdist <- 'data/cvjv_orig/ag_distribution_basins.csv'

# OUTPUTS
rawdat <- 'data/NASS_raw_statewide.csv'
agstats <- 'data/NASS_totals_statewide.csv'
results <- 'data/NASS_totals_cvjv.csv'


# NASS QUICKSTATS -------------
# Acquire annual ag statistics from NASS Quickstats
## extract annual totals of rice, corn, other field crops (beans, barley, oats,
##  safflower, sorghum, sugarbeets, wheat),  row crops (vegetable totals),
##  and alfalfa

dat <- do.call(bind_rows, lapply(c(2007:2017), extract_nass, cvjv.match = FALSE))
write_csv(dat, here::here(rawdat))

dat %>% ggplot(aes(as.numeric(year), Value/1000, color = as.factor(commodity_desc))) + 
  geom_line() + geom_point() + scale_color_viridis_d() + theme_classic() +
  scale_x_continuous(breaks = seq(2007, 2017, 1)) # remember these are state-wide totals

# group together the other field and row crops, and summarize by year
sdat <- read_csv(here::here(rawdat)) %>% 
  select(commodity_desc, 
         year, 
         acres = Value) %>%
  # match some values that were different in original NASS data used for CVJV
  mutate(acres = case_when(commodity_desc == 'VEGETABLE TOTALS' & year == 2014 & acres == 737300 ~ 750000, 
                           commodity_desc == 'COTTON' & year == 2010 & acres == 306000 ~ 303000,
                           commodity_desc == 'COTTON' & year == 2007 & acres == 455000 ~ 451000,
                           TRUE ~ acres)) %>%
  # group into crop classes
  mutate(crop_class = case_when(commodity_desc == 'CORN' ~ 'corn',
                                commodity_desc == 'RICE' ~ 'rice',
                                commodity_desc == 'VEGETABLE TOTALS' ~ 'rowcrop',
                                commodity_desc %in% c('BARLEY', 'OATS', 'WHEAT') ~ 'grains',
                                commodity_desc %in% c('BEANS', 'COTTON', 'MINT', 
                                                      'SAFFLOWER', 'SORGHUM', 
                                                      'SUGARBEETS', 'SUNFLOWER') ~ 'fieldcrop',
                                TRUE ~ NA_character_),
         year = as.numeric(year)) %>%
  filter(!is.na(acres)) %>%
  group_by(crop_class, year) %>%
  summarize(acres = sum(acres))

## check for missing values
sdat %>% spread(key = 'year', value = 'acres')

sdat %>% ggplot(aes(year, acres/1000, color = crop_class)) + 
  geom_line() + geom_point() + scale_color_viridis_d() + theme_classic() +
  scale_x_continuous(breaks = seq(2007, 2017, 1)) #statewide totals!

write_csv(sdat, here::here(agstats))

# ANNUAL CVJV TOTALS------------
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

mdat <- read_csv(here::here(agstats)) %>%
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
mdat %>% 
  filter(year <= 2014) %>%
  group_by(crop_class, year) %>%
  summarize(ha = sum(ha)) %>%
  group_by(crop_class) %>%
  summarize(ha = mean(ha))

# summarize total for each crop class in each year
tdat <- mdat %>% 
  group_by(crop_class, year) %>%
  summarize(ha = sum(ha)) 

write_csv(tdat, here::here(results))
