# README---------------
# Compile time series of habitat available and accessible from crops and 
# wetlands not enrolled in incentive programs. Because areas of these programs
# are relatively small, ignore them?
#
# STEP 2: update flooding curves
#
# PACKAGES
library(tidyverse)
library(gamm4)

# FUNCTIONS
# custom function to fit GAMMs to water tracker data
source(here::here('functions/fit_flooding_curves_gamm4.R'))


# INPUTS
tracker <- 'data/stats_basin_ag.csv' #data from water tracker (via Nathan)

# OUTPUTS
floodcurves <- 'output/open_water_annual.csv'
floodcurves_overall <- 'output/open_water_overall.csv'

# RAW DATA-------------
# from water tracker automated processing; cloud-filled!

dat <- read_csv(here::here(tracker), col_types = cols()) %>%
  # drop extra column
  select(-X12) %>% 
  # drop Suisun basin
  filter(BasinName != 'Suisun') %>%
  # drop crop classes we aren't using:
  filter(ClassName %in% c('Corn', 'Rice', 'Field Crop', 'Grains', 'Row Crop', 'Wetland')) %>%
  # group row, field, and grains into "Other"
  mutate(ClassName = recode(ClassName,
                            'Field Crop' = 'Other',
                            'Row Crop' = 'Other',
                            'Grains' = 'Other')) %>%
  mutate_at(vars(DataSource:Mosaic), as.factor) %>% 
  mutate_at(vars(MosaicDateStart:MosaicDateEnd), as.Date, format = '%m/%d/%Y') 
  

# OPEN WATER BY CROP CLASS----------
# summarize open water data by crop class over all basins (except Suisun)

sdat <- dat %>%
  # first back-calculate total area from percent observed and observed area
  mutate(TotalArea = ObservedArea / (PercentObserved/100)) %>%
  # drop non-existent Tulare Rice:
  filter(Name != 'Tulare Rice') %>%
  # drop unsuitable corn in Tulare and San Joaquin basins:
  filter(!(Name %in% c('Tulare Corn', 'San Joaquin Corn'))) %>%
  group_by(ClassName, Mosaic, MosaicDateStart, MosaicDateEnd) %>%
  # summarize and conver to number of pixels (30x30m)
  summarize(TotalArea = sum(TotalArea) / 900,
            ObservedArea = sum(ObservedArea) / 900,
            ObservedAreaWater = sum(ObservedAreaWater) / 900,
            PropObserved = ObservedArea/TotalArea,
            PropWater = ObservedAreaWater/ObservedArea) %>%
  ungroup() %>%
  # # filter dates to remove duplicates
  # mutate(monthpart = case_when(as.numeric(format(MosaicDateStart, '%d')) <= 15 & 
  #                                as.numeric(format(MosaicDateEnd, '%d')) <= 15 ~ 'A',
  #                              as.numeric(format(MosaicDateStart, '%d')) >= 16 &
  #                                as.numeric(format(MosaicDateEnd, '%d')) >= 16 ~ 'B',
  #                              MosaicDateStart %in% c('2015-10-08', '2015-10-31') ~ 'A',
  #                              MosaicDateStart %in% c('2014-12-15', '2015-11-16') ~ 'B',
  #                              TRUE ~ NA_character_)) %>%
  # filter(!is.na(monthpart)) %>%
  # add mid-point of mosaic date range:
  mutate(MosaicDateMid = MosaicDateStart + (MosaicDateEnd - MosaicDateStart)/2,
         month = as.numeric(format(MosaicDateMid, '%m')),
         yday = as.numeric(format(MosaicDateMid, '%j')),
         yday = case_when(month <= 6 ~ yday + 184,
                          month >= 7 ~ yday - 181),
         bioyear = as.numeric(format(MosaicDateMid, '%Y')),
         bioyear = case_when(month <=6 ~ bioyear - 1,
                             TRUE ~ bioyear)) %>%
  filter(bioyear >= 2013 & bioyear <= 2017) %>%
  mutate(label = recode(bioyear, 
                        '2013' = '2013-14',
                        '2014' = '2014-15',
                        '2015' = '2015-16',
                        '2016' = '2016-17',
                        '2017' = '2017-18'))

ggplot(sdat %>% filter(yday < 330), 
       aes(yday, PropWater, color = as.factor(label))) + 
  geom_point() + facet_wrap(~ClassName) 


# FIT GAMMS-------------
# same model calls as in CVJV work, but eliminate some duplicated scenes from
#  rolling mosaics

tmp <- sdat %>% filter(ClassName == 'Wetland' & yday <= 319) %>% 
  arrange(MosaicDateStart)
for (i in c(1:(nrow(tmp) - 1))) {
  if (tmp$MosaicDateEnd[i] >= tmp$MosaicDateStart[i+1]) tmp = tmp[-i,]
}  
wetlands <- fit_gamm4(tmp, 
                      dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea', 
                      nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater', 
                      minprop = 0.4, k = 20, by = 'label') 
wetlands_overall <- fit_gamm4(tmp, 
                              dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea', 
                              nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater', 
                              minprop = 0.4, k = 20) 


tmp <- sdat %>% filter(ClassName == 'Rice' & yday <= 319) %>% 
  arrange(MosaicDateStart)
for (i in c(1:(nrow(tmp) - 1))) {
  if (tmp$MosaicDateEnd[i] >= tmp$MosaicDateStart[i+1]) tmp = tmp[-i,]
} 
rice <- fit_gamm4(tmp,
                  dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea',
                  nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater',
                  minprop = 0.4, k = 20, by = 'label') 
rice_overall = fit_gamm4(tmp,
                         dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea',
                         nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater',
                         minprop = 0.4, k = 20) 

tmp <- sdat %>% filter(ClassName == 'Corn' & yday <= 319) %>% 
  arrange(MosaicDateStart)
for (i in c(1:(nrow(tmp) - 1))) {
  if (tmp$MosaicDateEnd[i] >= tmp$MosaicDateStart[i+1]) tmp = tmp[-i,]
} 
corn = fit_gamm4(tmp, 
                 dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea', 
                 nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater',
                 minprop = 0.4, k = 20, by = 'label')

corn_overall = fit_gamm4(tmp, 
                         dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea', 
                         nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater',
                         minprop = 0.4, k = 20) 

tmp <- sdat %>% filter(ClassName == 'Other' & yday <= 319) %>% 
  arrange(MosaicDateStart)
for (i in c(1:(nrow(tmp) - 1))) {
  if (tmp$MosaicDateEnd[i] >= tmp$MosaicDateStart[i+1]) tmp = tmp[-i,]
} 
other = fit_gamm4(tmp, 
                  dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea', 
                  nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater',
                  minprop = 0.4, k = 20, by = 'label') 
other_overall = fit_gamm4(tmp, 
                          dayofyear = 'yday', year = 'bioyear', ntotal = 'TotalArea', 
                          nsampled = 'ObservedArea', nflooded = 'ObservedAreaWater',
                          minprop = 0.4) 

## combine predicted values:
pred <- wetlands$pred %>% mutate(landcover = 'wetlands') %>%
  bind_rows(rice$pred %>% mutate(landcover = 'rice')) %>% 
  bind_rows(corn$pred %>% mutate(landcover = 'corn')) %>%
  bind_rows(other$pred %>% mutate(landcover = 'other')) %>% 
  mutate(landcover = as.factor(landcover)) 

pred_overall <- wetlands_overall$pred %>% mutate(landcover = 'wetlands') %>%
  bind_rows(rice_overall$pred %>% mutate(landcover = 'rice')) %>% 
  bind_rows(corn_overall$pred %>% mutate(landcover = 'corn')) %>%
  bind_rows(other_overall$pred %>% mutate(landcover = 'other')) %>% 
  mutate(landcover = as.factor(landcover)) 


write_csv(pred, here::here(floodcurves))
write_csv(pred_overall, here::here(floodcurves_overall))
