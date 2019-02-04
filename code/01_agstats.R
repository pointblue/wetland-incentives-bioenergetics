# README---------------
# Acquire annual ag statistics from NASS Quickstats

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

# OUTPUT
rawdat <- 'data/annual_NASS_raw_statewide.csv'
agstats <- 'data/annual_NASS_totals_statewide.csv'


# EXTRACT QUICKSTATS-------------
## extract annual totals of rice, corn, other field crops (beans, barley, oats,
##  safflower, sorghum, sugarbeets, wheat),  row crops (vegetable totals),
##  and alfalfa

dat <- do.call(bind_rows, lapply(c(2007:2017), extract_nass, cvjv.match = FALSE))
write_csv(dat, here::here(rawdat))

# SUMMARIZE------------------
## group together the other field and row crops, and summarize by year #MINT?
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

## plot (remember these are state-wide totals)
dat %>% ggplot(aes(as.numeric(year), Value/1000, color = as.factor(commodity_desc))) + 
  geom_line() + geom_point() + scale_color_viridis_d() + theme_classic() +
  scale_x_continuous(breaks = seq(2007, 2017, 1))
sdat %>% ggplot(aes(year, acres/1000, color = crop_class)) + 
  geom_line() + geom_point() + scale_color_viridis_d() + theme_classic() +
  scale_x_continuous(breaks = seq(2007, 2017, 1))

write_csv(sdat, here::here(agstats))
