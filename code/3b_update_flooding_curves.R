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
br_ts <- 'data/BR_timeseries.csv'
whep_ts <- 'data/WHEP_timeseries.csv'

# OUTPUTS
floodcurves <- 'output/open_water_annual.csv'
floodcurves_wateryear <- 'output/open_water_by_wateryear.csv'
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
# summarize open water data by crop class over all basins (except Suisun) in
#  each individual central valley mosaic

sdat <- dat %>%
  # first back-calculate total area from percent observed and observed area
  mutate(ObservedAreaWater = case_when(ObservedArea==0 & is.na(ObservedAreaWater) ~ 0,
                                       TRUE ~ ObservedAreaWater),
         TotalArea = case_when(ObservedArea == 0 ~ 0,
                               TRUE ~ ObservedArea / (PercentObserved/100))) %>%
  # drop non-existent Tulare Rice:
  filter(Name != 'Tulare Rice') %>%
  # drop unsuitable corn in Tulare and San Joaquin basins:
  filter(!(Name %in% c('Tulare Corn', 'San Joaquin Corn'))) %>%
  group_by(ClassName, Mosaic, MosaicDateStart, MosaicDateEnd) %>%
  # summarize and conver to number of pixels (30x30m)
  summarize(ntotal = sum(TotalArea) / 900,
            nsampled = sum(ObservedArea) / 900,
            nflooded = sum(ObservedAreaWater) / 900) %>%
  ungroup() %>%
  # bias correction
  mutate(nflooded2 = case_when(ClassName == 'Wetland' ~ nflooded * 1.11,
                               ClassName == 'Rice' ~ nflooded * 0.96,
                               ClassName == 'Corn' ~ nflooded * 0.95,
                               TRUE ~ nflooded)) %>%
  # add mid-point of mosaic date range:
  mutate(MosaicDateMid = MosaicDateStart + (MosaicDateEnd - MosaicDateStart)/2,
         month = as.numeric(format(MosaicDateMid, '%m')),
         yday = as.numeric(format(MosaicDateMid, '%j')),
         yday = case_when(month <= 6 ~ yday + 184,
                          month >= 7 ~ yday - 181),
         bioyear = as.numeric(format(MosaicDateMid, '%Y')),
         bioyear = case_when(month <=6 ~ bioyear - 1,
                             TRUE ~ bioyear)) %>%
  filter(bioyear >= 2013 & bioyear <= 2016 & yday <= 319) %>%
  mutate(label = recode(bioyear, 
                        '2013' = '2013-14',
                        '2014' = '2014-15',
                        '2015' = '2015-16',
                        '2016' = '2016-17'),
         wateryear = recode(bioyear,
                            '2013' = 'critical',
                            '2014' = 'critical',
                            '2015' = 'below normal',
                            '2016' = 'wet')) 

ggplot(sdat, aes(yday, nflooded/nsampled, color = label)) +
  geom_point() + facet_wrap(~ClassName) +
  geom_point(aes(y = nflooded2/nsampled), shape = 21)

ggplot(sdat %>% filter(ClassName == 'Wetland'), 
       aes(yday, nflooded2/nsampled, color = label)) +
  geom_smooth(aes(fill = label)) + geom_point() 

ggplot(sdat %>% filter(ClassName == 'Wetland'), 
       aes(yday, nflooded2/nsampled, color = wateryear)) +
  geom_smooth(aes(fill = wateryear)) + geom_point() 


# SUBTRACT INCENTIVE ACRES------------
br <- read_csv(here::here(br_ts)) %>%
  select(bioyear, yday, available) %>%
  filter(available > 0) %>%
  rename(br = available) %>%
  # convert from ha to 900m2 pixels
  mutate(br = (br * 10000)/900)

whep <- read_csv(here::here(whep_ts)) %>%
  select(bioyear, yday, available, landcover) %>%
  filter(available > 0) %>%
  mutate(available = (available * 10000)/900) %>%
  spread(key = landcover, value = available, fill = 0)

mdat <- sdat %>% 
  select(ClassName, bioyear, yday, ntotal, nsampled, nflooded, nflooded2, label, 
         wateryear) %>%
  left_join(br, by = c('bioyear', 'yday')) %>%
  left_join(whep, by = c('bioyear', 'yday')) %>%
  mutate(br = case_when(is.na(br) ~ 0,
                        TRUE ~ br),
         whep_fall = case_when(is.na(whep_fall) ~ 0,
                               TRUE ~ whep_fall),
         whep_vardd = case_when(is.na(whep_vardd) ~ 0,
                                TRUE ~ whep_vardd),
         nflooded3 = case_when(ClassName == 'Rice' ~ nflooded2 - br - whep_fall - whep_vardd,
                               TRUE ~ nflooded2))

ggplot(mdat, aes(yday, nflooded2/nsampled, color = label)) +
  geom_point() + facet_wrap(~ClassName) +
  # geom_point(aes(y = nflooded2/nsampled), shape = 21) +
  geom_point(aes(y = nflooded3/nsampled), shape = 21)


# FIT GAMMS-------------

by_class <- mdat %>% split(.$ClassName)

by_year <- by_class %>% 
  map_dfr(~ fit_gamm4(df = ., nwater = 'nflooded3', dayofyear = 'yday',
                      year = 'bioyear', minprop = 0.4, by = 'label', 
                      plot = FALSE)) %>%
  mutate(landcover = rep(levels(mdat$ClassName), 
                         each = nrow(.)/length(levels(mdat$ClassName))))

by_water_year <- by_class %>%
  map_dfr(~ fit_gamm4(df = ., nwater = 'nflooded3', dayofyear = 'yday',
                      year = 'bioyear', minprop = 0.4, by = 'wateryear', 
                      plot = FALSE)) %>%
  mutate(landcover = rep(levels(mdat$ClassName), 
                         each = nrow(.)/length(levels(mdat$ClassName))))

overall <- by_class %>%
  map_dfr(~ fit_gamm4(df = ., nwater = 'nflooded2', dayofyear = 'yday',
                      year = 'bioyear', minprop = 0.4, plot = FALSE)) %>%
  mutate(landcover = rep(levels(mdat$ClassName), 
                         each = nrow(.)/length(levels(mdat$ClassName))))

ggplot(by_year %>% filter(landcover == 'Wetland'), 
       aes(yday, fit)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = group), alpha = 0.5) +
  geom_line(aes(color = group)) +
  geom_point(data = mdat %>% filter(ClassName == 'Wetland'),
             aes(y = nflooded3/nsampled, color = label), shape = 21)

ggplot(by_water_year %>% filter(landcover == 'Wetland'), aes(yday, fit)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = group), alpha = 0.5) +
  geom_line(aes(color = group)) +
  geom_point(data = mdat %>% filter(ClassName == 'Wetland'),
             aes(y = nflooded3/nsampled, color = wateryear), shape = 21) +
  # add overall/average curve for comparison:
  geom_ribbon(data = overall %>% filter(landcover == 'Wetland'), 
              aes(ymin = lcl, ymax = ucl), fill = 'gray50', alpha = 0.5) +
  geom_line(data = overall %>% filter(landcover == 'Wetland'), color = 'black')
 
ggplot(by_year %>% filter(landcover == 'Rice'), 
       aes(yday, fit)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = group), alpha = 0.5) +
  geom_line(aes(color = group)) +
  geom_point(data = mdat %>% filter(ClassName == 'Rice'),
             aes(y = nflooded3/nsampled, color = label), shape = 21) +
  geom_point(data = mdat %>% filter(ClassName == 'Rice'),
             aes(y = nflooded2/nsampled, color = label), shape = 19)


## mid-winter peak values:
by_year %>% 
  group_by(landcover, group) %>%
  summarize(yday = yday[which(fit == max(fit[which(yday < 250)]))], #avoid late peaks in rice
            peak = max(fit),
            lcl = lcl[which(fit == max(fit))],
            ucl = ucl[which(fit == max(fit))])
# wetlands: 202-229, 0.604-0.646
# rice: 200-215, 0.424-0.609
# corn: 197-207, 0.242-0.260
# other: 174-191, 0.105-0.138

write_csv(by_year, here::here(floodcurves))
write_csv(by_water_year, here::here(floodcurves_wateryear))
write_csv(overall, here::here(floodcurves_overall))
