# README------------
# Extract annual/seasonal acres enrolled in Bird Returns program

# PACKAGES
library(tidyverse)
library(sf)

# INPUT DATA
brfields <- 'data/CONFIDENTIAL_br_fieldsMaster'

# OUTPUT DATA
br_enrollment <- 'data/BR_fields.csv'
br_ts <- 'data/BR_timeseries.csv'
br_table <- 'data/BR_enrollment_table.csv'
br_totals <- 'data/BR_totals.csv'

# PROCESS DATA--------------
shp <- st_read(here::here(brfields), 'CONFIDENTIAL_br_fieldsMaster') %>%
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

write_csv(dat, here::here(br_enrollment))


# EXPLORE & SUMMARIZE---------

# total acreage of all fields ever enrolled:
dat %>% select(polygonID:GIS_Ac) %>% 
  distinct() %>% 
  summarize(GIS_Ac = sum(GIS_Ac)/2.47105)
# 13,890 ha

# total acreage enrolled by year, season, and date range
# NOTE: there are still some date ranges that need to be filled in
dat %>%
  group_by(year, season, dates, opt) %>%
  summarize(GIS_Ac = sum(GIS_Ac)) %>% 
  print(n = 36)

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

# which seasons are fields enrolled in?
dat %>% 
  select(-dates, -opt) %>%
  unite('season', season, year) %>%
  spread(key = season, value = program)
#--> many enrolled in multiple seasons

# TIME SERIES---------------
# proportion of all BR acres (ever) enrolled on each day of the shorebird
# non-breeding season (where day 1 = 1 July) in each year
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

ts <- expand.grid(yday = c(1:320),
                  bioyear = c(2013:2017))
ts <- pmap_dfr(list(ts$yday, ts$bioyear),
               function(y, byear) {
                 res <- mdat %>% 
                   filter(ystart <= y & yend >= y & bioyear == byear) %>%
                   mutate(group = case_when(ystart == y ~ 'new',
                                            TRUE ~ 'existing')) %>%
                   group_by(group) %>%
                   summarize(ha = sum(GIS_Ac) / 2.47105) %>%
                   mutate(yday = y, bioyear = byear)
                 }
               ) %>%
  mutate(group = factor(group, levels = c('new', 'existing'))) %>%
  spread(key = group, value = ha) %>% 
  arrange(bioyear, yday)
write_csv(ts, here::here(br_ts))

ggplot(ts, aes(x = yday, y = existing), color = 'black') + geom_line() +
  geom_point(aes(y = new), color = 'red') + facet_wrap(~bioyear) + ylab('ha')
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



