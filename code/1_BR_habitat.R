# READ ME------------
# Compile time series of habitat available and accessible from fields enrolled 
# in Bird Returns program. Assume fields are 100% available (open water) and
# accessible (suitable shorebird depth) during enrollment.

# PACKAGES
library(tidyverse)
library(sf)

# INPUT DATA
brfields <- 'data/GIS/CONFIDENTIAL_br_fieldsMaster'

# OUTPUT DATA
br_enrollment <- 'data/BR_fields.csv'
br_ts <- 'data/BR_timeseries.csv'
br_table <- 'data/BR_enrollment_table.csv'
br_totals <- 'data/BR_totals.csv'

# PROCESS SPATIAL DATA--------------
# remove individual names and simplify fields

shp <- st_read(here::here(brfields), 'CONFIDENTIAL_br_fieldsMaster_20190226') %>%
  rename(d_sprg_2014 = d_sprg_201,
         d_fall_2014 = d_fall_201,
         d_wntr_2015 = d_wntr_201,
         d_sprg_2015 = d_sprg_202,
         d_fall_2015 = d_fall_202,
         d_sprg_2016 = d_sprg_203,
         d_fall_2016 = d_fall_203,
         d_sprg_2017 = d_sprg_204,
         d_fall_2017 = d_fall_204,
         d_sprg_2018 = d_sprg_205,
         d_fall_2018 = d_fall_205,
         wn15_opt = wn18_opt) %>% #fix error in field name
  mutate(polygonID = c(1:nrow(.))) %>%
  # remove encoded field names and property details
  select(polygonID, UniqueID, GIS_Ac, d_sprg_2014:d_fa18_dat) 

dat <- shp %>%
  st_set_geometry(NULL) %>%
  as.tibble() %>%
  gather(d_sprg_2014:d_fa18_dat, key = 'var', value = 'value') %>%
  mutate(var = gsub('^d_', '', var),
         var = gsub('_2014$', '_2014_program', var),
         var = gsub('_2015$', '_2015_program', var),
         var = gsub('_2016$', '_2016_program', var),
         var = gsub('_2017$', '_2017_program', var),
         var = gsub('_2018$', '_2018_program', var),
         var = gsub('fa14', 'fall_2014', var),
         var = gsub('fa15', 'fall_2015', var),
         var = gsub('fa16', 'fall_2016', var),
         var = gsub('fa17', 'fall_2017', var),
         var = gsub('fa18', 'fall_2018', var),
         var = gsub('sp14', 'spring_2014', var),
         var = gsub('sp15', 'spring_2015', var),
         var = gsub('sp16', 'spring_2016', var),
         var = gsub('sp17', 'spring_2017', var),
         var = gsub('sp18', 'spring_2018', var),
         var = gsub('wn15', 'winter_2015', var),
         var = gsub('wn18', 'winter_2018', var),
         var = gsub('sprg', 'spring', var),
         var = gsub('wntr', 'winter', var),
         var = gsub('dat$', 'dates', var),
         var = gsub('ID', 'id', var)) %>%
  separate(var, into = c('season', 'year', 'var')) %>%
  mutate(year = as.numeric(year),
         season = factor(season, levels = c('winter', 'spring', 'fall')),
         var = as.factor(var)) %>%
  filter(!is.na(value)) %>% 
  spread(key = 'var', value = 'value') %>%
  filter(program == 'enrolled') %>% #INCLUDE ONLY ENROLLED FIELDS
  arrange(year, season, polygonID) %>%
  mutate(dates = gsub('Feb 1-', 'Feb 1 -', dates),
         opt = gsub('Aug A', 'Aug B', opt),
         dates = case_when(opt == 'Aug B' & is.na(dates) ~ 'Aug 15 - Aug 28',
                           TRUE ~ dates)) #fix typo

dat %>% filter(is.na(dates)) #17 more rows with no dates; filled in later by Katie Andrews

dat <- dat %>%
  # believe not actually enrolled after all
  filter(!(UniqueID %in% c('yu2472', 'yu2416', 'yu1668', 'su954', 'yu2473') & season == 'fall' & year == 2014)) %>%
  # control fields (for surveys, not flooded)
  filter(!(UniqueID %in% c('yo3453', 'yo3454', 'yo3455', 'yo3464') & season == 'fall' & year == 2014)) %>%
  # add dates
  mutate(dates = case_when(UniqueID %in% c('yu2465', 'yu2469', 'yu1670', 'yu1669', 'yo3463', 'yu2463', 'yo3447') & 
                             season == 'fall' & year == '2014' ~ 'Oct 18 - Oct 31',
                           UniqueID == 'yo3447' & season == 'spring' & year == '2014' ~ 'Mar 10 - Apr 4',
                           TRUE ~ dates),
         opt = case_when(UniqueID %in% c('yu2465', 'yu2469', 'yu1670', 'yu1669', 'yo3463', 'yu2463', 'yo3447') & 
                           season == 'fall' & year == '2014' ~ 'Oct B',
                         UniqueID == 'yo3447' & season == 'spring' & year == '2014' ~ 'Mar B',
                         TRUE ~ opt))

dat %>% filter(is.na(dates))  # none

write_csv(dat, here::here(br_enrollment))


# EXPLORE & SUMMARIZE---------

# total acreage of all fields ever enrolled:
dat %>% select(polygonID:GIS_Ac) %>% 
  distinct() %>% 
  summarize(GIS_Ac = sum(GIS_Ac)/2.47105)
# 15,207 ha

# total acreage enrolled by year, season, and date range
dat %>%
  group_by(year, season, dates, opt) %>%
  summarize(GIS_Ac = sum(GIS_Ac)) %>% 
  print(n = 45)

# summarize number of times each field enrolled
dat %>%
  group_by(polygonID, UniqueID, GIS_Ac, season) %>%
  tally() %>% 
  group_by(polygonID, UniqueID, GIS_Ac) %>%
  add_tally() %>%
  rename(total = nn) %>%
  spread(key = season, value = n, fill = 0) %>%
  summary()
# 1 - 5 times total


# TIME SERIES---------------
# start and end dates of enrollment for each field in each season/year
mdat <- dat %>% 
  # change date range text into start and end dates
  separate(dates, into = c('start', 'end'), sep = ' - ') %>%
  mutate(start = as.Date(paste(year, start), format = '%Y %b %d'),
         end = as.Date(paste(year, end), format = '%Y %b %d')) %>%
  # calculate day of (calendar) year
  mutate(ystart = as.numeric(format(start, '%j')),
         yend = as.numeric(format(end, '%j'))) %>%
  # adjust for bioyear (Jul to Jun) -- no date ranges span Jun 30-Jul 1
  mutate(startm = as.numeric(format(start, '%m')),
         endm = as.numeric(format(end, '%m')),
         bioyear = case_when(startm >=1 & endm <=6 ~ year - 1,
                             TRUE ~ year),
         ystart = case_when(startm <= 6 ~ ystart + 184,
                            startm >= 7 ~ ystart - 181),
         yend = case_when(endm <= 6 ~ yend + 184,
                          endm >= 7 ~ yend - 181))

# for each day of each bioyear, identify total area of fields just added that 
#   day ('added'), currently flooded ('available'), or went dry that day 
#   ('returned'); 
#   --> assuming it takes 2 weeks from end of contract for field to fully dry
df <- expand.grid(yday = c(1:319),
                  bioyear = c(2013:2016))

ts <- pmap_dfr(list(df$yday, df$bioyear),
               function(y, byear) {
                 res <- mdat %>% 
                   filter(ystart <= y & yend >= y - 14 & bioyear == byear) %>%
                   mutate(group = case_when(ystart == y ~ 'added',
                                            yend + 14 == y ~ 'returned', # assume 2 weeks to dry out
                                            TRUE ~ 'available')) %>%
                   group_by(group) %>%
                   summarize(ha = sum(GIS_Ac) / 2.47105) %>%
                   mutate(yday = y, bioyear = byear)
                 }
               ) 

# data frame of daily change in area available, accessible, newly added and 
#   returned, treating fall and spring enrollments as separate land cover types
#   (lump one winter season with spring)
change <- ts %>%
  mutate(habitat = case_when(yday < 185 ~ 'br_fall',
                             yday >= 185 ~ 'br_spring')) %>%
  complete(bioyear, habitat, yday = 1:319, 
           fill = list(group = 'available', ha = 0)) %>%
  spread(key = group, value = ha) %>% 
  arrange(habitat, bioyear, yday) %>%
  # if returned or added, also consider it available that day; if returned
  #   or added, and other acres already available that day, add them together
  #  (Note: there are no days where all 3 are filled in, or where acres are both
  #   added and returned on the same day)
  mutate(available = case_when(!is.na(available) & !is.na(returned) ~ returned + available,
                               !is.na(available) & !is.na(added) ~ added + available,
                               is.na(available) & !is.na(added) ~ added,
                               is.na(available) & !is.na(returned) ~ returned,
                               is.na(added) & is.na(returned) ~ available),
         added = case_when(is.na(added) ~ 0,
                           TRUE ~ added),
         returned = case_when(is.na(returned) ~ 0,
                              TRUE ~ returned),
         # assume all BR acres are fully accessible to shorebirds (shallow)
         accessible = available,
         prop.accessible = 1,
         group = recode(bioyear, 
                        '2013' = '2013-14',
                        '2014' = '2014-15',
                        '2015' = '2015-16',
                        '2016' = '2016-17')) %>%
  select(habitat, group, yday, available, accessible, added, returned, prop.accessible)
  
write_csv(change, here::here(br_ts))

ggplot(change, aes(x = yday, y = available), color = 'black') + 
  geom_line(aes(color = habitat)) +
  geom_point(data = change %>% mutate(added = case_when(added == 0 ~ NA_real_,
                                                        TRUE ~ added)),
                                      aes(y = added), color = 'green') + 
  geom_point(data = change %>% mutate(returned = case_when(returned == 0 ~ NA_real_,
                                                           TRUE ~ returned)),
             aes(y = available - returned), color = 'red') +
  facet_wrap(~group) + ylab('ha') + xlab('day of year (1 = 1 July)') 
# red points are dates when new acres were added; black line shows existing 
# (previously enrolled acres)

# Note: fall program date ranges don't overlap, so assume all new acres
#  established for each option in the program (same field never enrolled under
#  two options in the same season)

# ENROLLMENT TABLE---------
# chart of which half-months each field was enrolled in each year; can be
# combined with polygons to pull out fields to include in seasonal land cover
# rasters if needed

sdat <- dat %>%
  # collapse dates and opt into fewer categories that apply over all years
  mutate(opt = case_when(season == 'winter' ~ 'Jan4',
                         dates == 'Feb 1 - Feb 14' ~ 'Feb2',
                         dates == 'Feb 1 - Feb 28' ~ 'Feb4',
                         dates == 'Feb 1- Feb 28' ~ 'Feb4',
                         dates == 'Feb 1 - Mar 14' ~ 'Feb6',
                         dates == 'Feb 1 - Mar 28' ~ 'Feb8',
                         dates == 'Feb 1 - Apr 14' ~ 'Feb10',
                         dates == 'Aug 15 - Aug 28' ~ 'Aug2B',
                         dates == 'Aug 15 - Sep 11' ~ 'Aug4B',
                         dates == 'Sep 1 - Sep 14' ~ 'Sep2A',
                         dates == 'Sep 2 - Sep 15' ~ 'Sep2A', #treat as the same?
                         dates == 'Sep 1 - Sep 28' ~ 'Sep4A',
                         dates == 'Sep 15 - Sep 28' ~ 'Sep2B',
                         dates == 'Sep 16 - Sep 30' ~ 'Sep2B', #treat as the same?
                         dates == 'Sep 17 - Oct 14' ~ 'Sep4B',
                         dates == 'Oct 1 - Oct 14' ~ 'Oct2A',
                         dates == 'Oct 4 - Oct 17' ~ 'Oct2A', #treat as the same?
                         dates == 'Oct 1 - Oct 28' ~ 'Oct4A',
                         dates == 'Oct 15 - Oct 28' ~ 'Oct2B',
                         dates == 'Oct 18 - Oct 31' ~ 'Oct2B', #treat as the same?
                         TRUE ~ opt)) %>%
  # add fields marking whether or not enrolled in the first/second halves of the month
  mutate(jan_a = case_when(opt %in% c('Jan4') ~ 1,
                           TRUE ~ 0),
         jan_b = case_when(opt %in% c('Jan4') ~ 1,
                           TRUE ~ 0),
         feb_a = case_when(opt %in% c('Feb2', 'Feb4', 'Feb6', 'Feb8', 'Feb10') ~ 1,
                           TRUE ~ 0),
         feb_b = case_when(opt %in% c('Feb4', 'Feb6', 'Feb8', 'Feb10') ~ 1,
                           TRUE ~ 0), 
         mar_a = case_when(opt %in% c('Feb6', 'Feb8', 'Feb10') ~ 1,
                           TRUE ~ 0),
         mar_b = case_when(opt %in% c('Feb8', 'Feb10') ~ 1,
                           TRUE ~ 0),
         apr_a = case_when(opt %in% c('Feb10') ~ 1,
                           TRUE ~ 0),
         aug_b = case_when(opt %in% c('Aug2B', 'Aug4B') ~ 1,
                           TRUE ~ 0),
         sep_a = case_when(opt %in% c('Aug4B', 'Sep2A', 'Sep4A') ~ 1,
                           TRUE ~ 0),
         sep_b = case_when(opt %in% c('Sep4A', 'Sep2B', 'Sep4B') ~ 1,
                           TRUE ~ 0),
         oct_a = case_when(opt %in% c('Sep4B', 'Oct2A', 'Oct4A') ~ 1,
                           TRUE ~ 0),
         oct_b = case_when(opt %in% c('Oct4A', 'Oct2B') ~ 1,
                           TRUE ~ 0)) %>%
  # summarize acres by year/half-month
  select(-season, -dates, -opt, -program) 
write_csv(sdat, here::here(br_table))

# TOTAL ACREAGE ENROLLED---------------
# total acreage by half-month:
tdat <- sdat %>%
  gather(jan_a:oct_b, key = 'timing', value = 'value') %>%
  mutate(timing = factor(timing, levels = c('jan_a', 'jan_b', 'feb_a', 'feb_b',
                                            'mar_a', 'mar_b', 'apr_a', 'aug_b',
                                            'sep_a', 'sep_b', 'oct_a', 'oct_b'))) %>%
  filter(value == 1) %>%
  group_by(year, timing) %>%
  summarize(ha = sum(GIS_Ac) / 2.47105) %>%
  spread(key = timing, value = ha, fill = 0)

write_csv(tdat, here::here(br_totals)) # Note: still missing some with no dates



