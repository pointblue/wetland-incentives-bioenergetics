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
agstats <- 'data/annual_NASS_totals_statewide.csv'


# NASS QUICKSTATS-------------
## extract annual totals of rice, corn, other field crops (beans, barley, oats,
##  safflower, sorghum, sugarbeets, wheat),  row crops (vegetable totals),
##  and alfalfa

dat <- do.call(bind_rows, lapply(c(2007:2017), extract_nass))

## add irrigated pasture data (from 2013 census)
dat2 <- bind_rows(dat, 
                  nass_data(source_desc = 'CENSUS', 
                            commodity_desc = 'PASTURELAND',
                            domain_desc = 'TOTAL',
                            statisticcat_desc = 'AREA',
                            unit_desc = 'ACRES',
                            agg_level_desc = 'STATE',
                            state_name = 'CALIFORNIA', 
                            token = Sys.getenv('NASS_TOKEN'))) 

## Note: alfalfa acres planted may not reflect total acreage since it's
##  a perennial

## group together the other field and row crops, and summarize by year
sdat <- dat2 %>% 
  select(commodity_desc, 
         year, 
         acres = Value) %>%
  mutate(crop_class = case_when(commodity_desc == 'CORN' ~ 'corn',
                                commodity_desc == 'RICE' ~ 'rice',
                                commodity_desc == 'HAY & HAYLAGE' ~ 'alfalfa',
                                commodity_desc == 'PASTURELAND' ~ 'pasture',
                                commodity_desc == 'VEGETABLE TOTALS' ~ 'rowcrop',
                                commodity_desc %in% c('BARLEY', 'OATS', 'WHEAT') ~ 'grains',
                                commodity_desc %in% c('BEANS', 'COTTON', 'SAFFLOWER', 'SORGHUM', 'SUGARBEETS', 'SUNFLOWER') ~ 'fieldcrop',
                                TRUE ~ NA_character_),
         year = as.numeric(year)) %>%
  group_by(crop_class, year) %>%
  summarize(acres = sum(acres))

## plot (remember these are state-wide totals)
sdat %>% ggplot(aes(year, acres/1000, color = crop_class)) + 
  geom_line() + geom_point() + scale_color_viridis_d() + theme_classic() +
  scale_x_continuous(breaks = seq(2007, 2017, 1))

write_csv(sdat, here::here(agstats))
