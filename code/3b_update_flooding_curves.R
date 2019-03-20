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
tracker <- 'data/stats_basin_ag.csv' #cloud-filled data from water tracker (via Nathan)
tracker_meta <- 'data/basin_stats_unfilled.csv' #basin-specific metadata (via Nathan)
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
  mutate_at(vars(MosaicDateStart:MosaicDateEnd), as.Date, format = '%m/%d/%Y',
            vars(DataSource:Mosaic), as.factor) %>%
  # drop Suisun basin
  filter(BasinName != 'Suisun') %>%
  # drop crop classes we aren't using:
  filter(ClassName %in% c('Corn', 'Rice', 'Field Crop', 'Grains', 'Row Crop', 'Wetland')) %>%
  # drop years we aren't using
  filter(MosaicDateStart < '2018-01-01')

# estimated total pixels of each crop in each basin: 
#  (from PercentObserved and ObservedArea)
totals <- dat %>% 
  # drop non-existent Tulare Rice:
  filter(Name != 'Tulare Rice') %>%
  # largest number of pixels observed for each crop class in each basin
  group_by(BasinName, ClassName) %>%
  summarize(basincroptotal = max(ObservedArea),
            percent = max(PercentObserved, na.rm = T)) %>% 
  # adjust upward for few that were never 100% observed:
  mutate(basincroptotal = basincroptotal / (percent/100),
         percent = percent / (percent/100)) %>%
  # group row, field, and grains into "Other"
  mutate(ClassName = recode(ClassName,
                            'Field Crop' = 'other',
                            'Row Crop' = 'other',
                            'Grains' = 'other',
                            'Wetland' = 'wetlands'),
         ClassName = tolower(ClassName)) %>%
  group_by(ClassName, BasinName) %>%
  summarize(basincroptotal = sum(basincroptotal)) %>%
  filter(!(ClassName == 'corn' & BasinName %in% c('San Joaquin', 'Tulare'))) %>%
  group_by(ClassName) %>%
  mutate(croptotal = sum(basincroptotal),
         basinprop.croptotal = basincroptotal/croptotal) %>%
  ungroup()


# META DATA--------------
# percent of each basin actually visible (before cloud-filling)
meta <- read_csv(here::here(tracker_meta), col_types = cols()) %>%
  filter(!is.na(BasinName) & BasinName != 'Suisun') %>%
  mutate(PercentObserved = case_when(is.na(PercentObserved) & ObservedArea == 0 ~ 0,
                                     TRUE ~ PercentObserved))

# for each mosaic, estimate cloud-free proportion of pixels, weighted by the 
#   proportion of each crop class in each basin
metadat <- bind_rows(
  # corn
  meta %>%
    #exclude unsuitable corn in San Joaquin and Tulare
    filter(!(BasinName %in% c('San Joaquin', 'Tulare'))) %>%
    left_join(totals %>% filter(ClassName == 'corn') %>% 
                select(BasinName, basinprop.croptotal), by = 'BasinName') %>%
    group_by(Mosaic) %>%
    summarize(ClassName = 'corn',
              prop.cloudfree = sum(basinprop.croptotal * PercentObserved)/100),
  # other crops
  meta %>%
    left_join(totals %>% filter(ClassName == 'other') %>% 
                select(BasinName, basinprop.croptotal), by = 'BasinName') %>%
    group_by(Mosaic) %>%
    summarize(ClassName = 'other',
              prop.cloudfree = sum(basinprop.croptotal * PercentObserved)/100),
  # rice
  meta %>%
    filter(BasinName != 'Tulare') %>%
    left_join(totals %>% filter(ClassName == 'rice') %>% 
                select(BasinName, basinprop.croptotal), by = 'BasinName') %>%
    group_by(Mosaic) %>%
    summarize(ClassName = 'rice',
              prop.cloudfree = sum(basinprop.croptotal * PercentObserved)/100),
  # wetlands
  meta %>%
    left_join(totals %>% filter(ClassName == 'wetlands') %>% 
                select(BasinName, basinprop.croptotal), by = 'BasinName') %>%
    group_by(Mosaic) %>%
    summarize(ClassName = 'wetlands',
              prop.cloudfree = sum(basinprop.croptotal * PercentObserved)/100)
)
ggplot(metadat, aes(prop.cloudfree)) + geom_density() + facet_wrap(~ClassName)


# OPEN WATER BY CROP CLASS AND DATE----------

# summarize cloud-filled open water data by crop class in each mosaic
basindat <- dat %>%
  # drop non-existent Tulare Rice:
  filter(Name != 'Tulare Rice') %>%
  
  # drop unsuitable corn in Tulare and San Joaquin basins:
  filter(!(Name %in% c('Tulare Corn', 'San Joaquin Corn'))) %>%
  
  # group row, field, and grains into "Other"
  mutate(ClassName = recode(ClassName,
                            'Field Crop' = 'other',
                            'Row Crop' = 'other',
                            'Grains' = 'other',
                            'Wetland' = 'wetlands'),
         ClassName = tolower(ClassName)) %>%
  
  group_by(ClassName, BasinName, Mosaic, MosaicDateStart, MosaicDateEnd) %>%
  summarize(nflooded = sum(ObservedAreaWater, na.rm = TRUE),
            nsampled = sum(ObservedArea)) %>%

  # add crop-specific totals:
  left_join(totals, by = c('BasinName', 'ClassName'))


# summarize cloud-filled open water data by crop class over all basins
#  (also convert to number of 30x30m pixels):
cropdat <- basindat %>%
  # summarize by crop class and mosaic date over all basins 
  group_by(ClassName, Mosaic, MosaicDateStart, MosaicDateEnd) %>%
  summarize(nflooded = sum(nflooded) / 900,
            nsampled = sum(nsampled) / 900,
            ntotal = sum(basincroptotal) / 900, #same as croptotal
            prop.sampled = nsampled / ntotal) %>%
  ungroup() 

# bias correction (from Reiter et al. paper)
cropdat_unbiased <- cropdat %>%
  mutate(nflooded = case_when(ClassName == 'wetlands' ~ nflooded * 1.11,
                              ClassName == 'rice' ~ nflooded * 0.96,
                              ClassName == 'corn' ~ nflooded * 0.95,
                              TRUE ~ nflooded))

# finalize data for analysis
sdat <- cropdat_unbiased %>%
  # add meta-data for mosaic image quality
  mutate(Mosaic = gsub('_cf10yr.tif', '.tif', Mosaic)) %>% #match mosaic names
  left_join(metadat, by = c('Mosaic', 'ClassName')) %>%

  # add mid-point of mosaic date range, assign day of year and bioyear labels:
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

ggplot(sdat, aes(prop.sampled)) + 
  facet_wrap(~ClassName, scales = 'free_y') + 
  geom_density()

ggplot(sdat, aes(prop.cloudfree)) + 
  facet_wrap(~ClassName, scales = 'free_y') + 
  geom_density()

ggplot(sdat %>% filter(prop.sampled >= 0.75 & prop.cloudfree >= 0.40), 
       aes(yday, nflooded/nsampled, color = group, size = prop.cloudfree)) +
  geom_point(alpha = 0.5) + facet_wrap(~ClassName) 


# SUBTRACT INCENTIVE ACRES------------
incentives <- bind_rows(read_csv(here::here(br_ts), col_types = cols()), 
                        read_csv(here::here(whep_ts), col_types = cols())) %>%
  select(group, yday, incentives = available) %>%
  group_by(group, yday) %>% 
  summarize(incentives = sum(incentives) * 10000 / 900) # convert to m2, then 30x30m pixels

# estimate nflooded_free (number of pixels flooded not through incentive programs)
mdat <- sdat %>% 
  select(habitat = ClassName, bioyear, group, yday, ntotal, nsampled, nflooded,
         prop.sampled, prop.cloudfree) %>%
  left_join(incentives, by = c('group', 'yday')) %>%
  mutate(nflooded_free = case_when(habitat == 'rice' ~ nflooded - incentives,
                                   TRUE ~ nflooded),
         habitat = factor(habitat, levels = c('wetlands', 'rice', 'corn', 'other')))

# check that only rice was affected
ggplot(mdat, aes(yday, nflooded/nsampled, color = group)) +
  geom_point() + facet_wrap(~habitat) +
  geom_point(aes(y = nflooded_free/nsampled), shape = 21)


# FIT GAMMS-------------
# proportion flooded by day of year, excluding incentive acres

by_year <- mdat %>% 
  filter(prop.sampled >= 0.7) %>% 
  split(.$habitat) %>% 
  map(~ fit_gamm4(df = ., nwater = 'nflooded_free', dayofyear = 'yday',
                  year = 'bioyear', weights = 'prop.cloudfree', 
                  by = 'group', plot = FALSE)) 

# extract predicted values:
by_year_pred <- by_year %>%
  map_dfr(function(x) {x$pred}, .id = 'habitat') %>%
  mutate(habitat = factor(habitat, levels = c('wetlands', 'rice', 'corn', 'other')))

ggplot(by_year_pred, aes(yday, fit)) + facet_wrap(~habitat, nrow = 4) + 
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = group), alpha = 0.5) +
  geom_line(aes(color = group)) +
  geom_point(data = mdat,
             aes(y = nflooded_free/nsampled, color = group, size = prop.cloudfree), 
             shape = 21) +
  ylim(0, 1) +
  scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                     labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                'Feb', 'Mar', 'Apr', 'May'))
# compared to CVJV estimates: 
# - wetlands curves all peak a bit lower than previously estimated
# - rice curves vary a lot between years, but look fairly similar overall
# - corn is very similar
# - "other" is much higher than before... is this real or an artifact of the cloud-fill model?

ggplot(by_year_pred %>% filter(habitat == 'other'), aes(yday, fit)) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = group), alpha = 0.5) +
  geom_line(aes(color = group)) +
  geom_point(data = mdat %>% filter(habitat == 'other'),
             aes(y = nflooded_free/nsampled, color = group), shape = 21) +
  facet_wrap(~habitat, nrow = 4) + ylim(0, 1) +
  scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                     labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                'Feb', 'Mar', 'Apr', 'May'),
                     expand = c(0, 0))


## mid-winter peak values:
by_year_pred %>% 
  group_by(habitat, group) %>%
  summarize(yday = yday[which(fit == max(fit[which(yday < 250)]))], #avoid late peaks in rice
            peak = max(fit),
            lcl = lcl[which(fit == max(fit))],
            ucl = ucl[which(fit == max(fit))])
# wetlands: 203-229, 0.604-0.646 (compared to 0.81 on day 199 in previous CVJV work)
# rice: 199-214, 0.396-0.612 (compared to 0.69 on day 188)
# corn: 197-208, 0.241-0.249 (compared to 0.22 on day 223)
# other: 167-195, 0.101-0.142 (compared to 0.03)

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
