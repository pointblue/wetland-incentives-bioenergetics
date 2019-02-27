# README---------------
# Compile time series of habitat available and accessible from crops and 
# wetlands not enrolled in incentive programs.
#
# STEP 2: update flooding curves (proportion of each land cover with open water
#   on each day of the nonbreeding season)
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
models <- 'output/flooding_curve_models.RData'
resamples <- 'output/flooding_curve_resamples.RData'

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
                            'Grains' = 'Other',
                            'Wetland' = 'Wetlands'),
         ClassName = tolower(ClassName)) %>%
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
  mutate(nflooded2 = case_when(ClassName == 'wetlands' ~ nflooded * 1.11,
                               ClassName == 'rice' ~ nflooded * 0.96,
                               ClassName == 'corn' ~ nflooded * 0.95,
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
  mutate(group = recode(bioyear, 
                        '2013' = '2013-14',
                        '2014' = '2014-15',
                        '2015' = '2015-16',
                        '2016' = '2016-17')) 

ggplot(sdat, aes(yday, nflooded/nsampled, color = group)) +
  geom_point() + facet_wrap(~ClassName) +
  geom_point(aes(y = nflooded2/nsampled), shape = 21)

ggplot(sdat %>% filter(ClassName == 'wetlands'), 
       aes(yday, nflooded2/nsampled, color = group)) +
  geom_smooth(aes(fill = group)) + geom_point() 


# SUBTRACT INCENTIVE ACRES------------
br <- read_csv(here::here(br_ts), col_types = cols()) %>%
  select(group, yday, available) %>%
  filter(available > 0) %>%
  rename(br = available) %>%
  # convert from ha to 900m2 pixels
  mutate(br = (br * 10000)/900)

whep <- read_csv(here::here(whep_ts), col_types = cols()) %>%
  select(group, yday, available, habitat) %>%
  filter(available > 0) %>%
  mutate(available = (available * 10000)/900) %>%
  spread(key = habitat, value = available, fill = 0)

mdat <- sdat %>% 
  select(ClassName, bioyear, group, yday, ntotal, nsampled, nflooded, nflooded2) %>%
  left_join(br, by = c('group', 'yday')) %>%
  left_join(whep, by = c('group', 'yday')) %>%
  mutate(br = case_when(is.na(br) ~ 0,
                        TRUE ~ br),
         whep_fall = case_when(is.na(whep_fall) ~ 0,
                               TRUE ~ whep_fall),
         whep_vardd = case_when(is.na(whep_vardd) ~ 0,
                                TRUE ~ whep_vardd),
         nflooded3 = case_when(ClassName == 'rice' ~ nflooded2 - br - whep_fall - whep_vardd,
                               TRUE ~ nflooded2))

ggplot(mdat, aes(yday, nflooded2/nsampled, color = group)) +
  geom_point() + facet_wrap(~ClassName) +
  # geom_point(aes(y = nflooded2/nsampled), shape = 21) +
  geom_point(aes(y = nflooded3/nsampled), shape = 21)


# FIT GAMMS-------------

by_year <- mdat %>% split(.$ClassName) %>% 
  map(~ fit_gamm4(df = ., nwater = 'nflooded3', dayofyear = 'yday',
                      year = 'bioyear', minprop = 0.4, by = 'group', 
                      plot = FALSE)) 

# extract predicted values:
by_year_pred <- by_year %>%
  map_dfr(function(x) {x$pred}) %>%
  mutate(habitat = rep(levels(as.factor(mdat$ClassName)), 
                       each = nrow(.)/length(levels(as.factor(mdat$ClassName)))))

ggplot(by_year_pred %>% filter(habitat == 'wetlands'), aes(yday, fit)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = group), alpha = 0.5) +
  geom_line(aes(color = group)) +
  geom_point(data = mdat %>% filter(ClassName == 'wetland'),
             aes(y = nflooded3/nsampled, color = group), shape = 21)

## mid-winter peak values:
by_year_pred %>% 
  group_by(habitat, group) %>%
  summarize(yday = yday[which(fit == max(fit[which(yday < 250)]))], #avoid late peaks in rice
            peak = max(fit),
            lcl = lcl[which(fit == max(fit))],
            ucl = ucl[which(fit == max(fit))])
# wetlands: 202-229, 0.604-0.646
# rice: 200-215, 0.424-0.609
# corn: 197-207, 0.242-0.260
# other: 174-191, 0.105-0.138

# SPLIT WETLANDS----------
# estimate percent of flooded wetlands that are semi-permanent vs. seasonal

## per Craig Isola: 
##  - seasonal wetlands start getting water in early Aug, so assume 100% of 
##      any wetlands before early August is a semi-permanent wetland (assumed 
##      to be day 34 in original CVJV work)
##  - peak of dryness in semi-permanent wetlands is mid-August through 
##      mid-October (e.g. day 107), so assume extent of flooded semi-permanent 
##      wetlands remains constant from early Aug (before seasonal wetlands
##      have any water) 
##  - all semi-permanent wetlands are fully flooded by early November 
##      (e.g. day 124), so from early Nov through May (end of season) assume: 
##      proportion of open water in wetlands that is semi-perm = 
##        total perm / prop.flooded*total wetlands
# -> fill in gap between day 107 and day 124 during flood-up in semi-permanent
#    wetlands with a spline
totalwetlands <- 74835.2959
semipermwetlands <- 6896.1698

by_year_wetsplit <- by_year_pred %>%
  group_by(group, habitat) %>%
  mutate(minperm = fit[yday == 34],
         prop.perm = case_when(yday <= 34 ~ 1,
                               yday > 34 & yday <= 107 ~ 
                                 (minperm * totalwetlands)/(fit * totalwetlands),
                               yday >= 124 ~ 
                                 semipermwetlands/(fit * totalwetlands),
                               yday > 107 & yday < 124 ~
                                 NA_real_,
                               TRUE ~ 0),
         spline = spline(x = yday, y = prop.perm, method = 'natural', 
                         xout = c(1:319))$y,
         prop.perm = case_when(yday > 107 & yday < 124 ~ spline,
                               TRUE ~ prop.perm),
         prop.perm = case_when(prop.perm > 1 ~ 1,
                               TRUE ~ prop.perm),
         minperm = NULL,
         spline = NULL) %>%
  ungroup() %>%
  mutate(prop.perm = case_when(habitat != 'wetlands' ~ NA_real_,
                               TRUE ~ prop.perm))

## check curves:
axes = list(scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                               labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                          'Feb', 'Mar', 'Apr', 'May')),
            xlab(NULL),
            ylab('Proportion open water'))

ggplot(by_year_wetsplit %>% filter(habitat == 'wetlands'), 
       aes(yday, fit, ymin = lcl, ymax = ucl)) + 
  geom_ribbon(fill = 'gray80') + geom_line() +
  geom_line(aes(y = prop.perm), color='red', size = 1.5) + 
  axes + facet_wrap(~group)


# RESAMPLES--------------
# generate resamples of proportion open water from GAMMs for use with Monte 
#   Carlo simulations; result should be a named list with elements for each
#   landcover type; each list contains estimates for each time step (rows) and
#   simulation (columns)

floodsim <- by_year %>%
  map(function(x) {
    sim <- plogis(x$Xp %*% t(x$br))
  })

# visualize as an example:
apply(floodsim$rice, 1, quantile, c(0.025, 0.5, 0.975)) %>% t() %>% 
  as.data.frame() %>% 
  bind_cols(by_year$rice$pred %>% select(yday, group)) %>% 
  rename(lcl = '2.5%', median = '50%', ucl = '97.5%') %>% 
  ggplot(aes(yday, median, ymin = lcl, ymax = ucl, fill = group, color = group)) + 
  geom_ribbon(alpha = 0.5) + geom_line()

## split wetlands in the resamples too:
floodsim$prop.perm <- floodsim$wetlands %>% as.tibble() %>%
  gather(key = 'iteration', value = 'fit') %>%
  mutate(yday = rep(rep(c(1:319), 4), 10000),
         group = rep(rep(c('2013-14', '2014-15', '2015-16', '2016-17'), each = 319), 10000)) %>%
  group_by(iteration, group) %>%
  mutate(minperm = fit[yday == 34],
         prop.perm = case_when(yday <= 34 ~ 1,
                               yday > 34 & yday <= 107 ~ 
                                 (minperm * totalwetlands)/(fit * totalwetlands),
                               yday >= 124 ~ 
                                 semipermwetlands/(fit * totalwetlands),
                               yday > 107 & yday < 124 ~
                                 NA_real_,
                               TRUE ~ 0),
         spline = spline(x = yday, y = prop.perm, method = 'natural', 
                         xout = c(1:319))$y,
         prop.perm = case_when(yday > 107 & yday < 124 ~ spline,
                               TRUE ~ prop.perm),
         prop.perm = case_when(prop.perm > 1 ~ 1,
                               TRUE ~ prop.perm),
         minperm = NULL,
         spline = NULL, 
         fit = NULL) %>%
  ungroup() %>%
  spread(key = 'iteration', value = 'prop.perm') %>%
  arrange(group, yday) %>%
  select(-yday, -group) %>%
  as.matrix()

# fix dimnames
dimnames(floodsim$prop.perm) <- dimnames(floodsim$wetlands)

# visualize to double-check:
apply(floodsim$prop.perm, 1, quantile, c(0.025, 0.5, 0.975)) %>% t() %>% 
  as.data.frame() %>% 
  bind_cols(by_year$rice$pred %>% select(yday, group)) %>% 
  rename(lcl = '2.5%', median = '50%', ucl = '97.5%') %>% 
  ggplot(aes(yday, median, ymin = lcl, ymax = ucl, fill = group, color = group)) + 
  geom_ribbon(alpha = 0.5) + geom_line()

write_csv(by_year_wetsplit, here::here(floodcurves))
save(by_year, file = here::here(models))
save(floodsim, file = here::here(resamples))
