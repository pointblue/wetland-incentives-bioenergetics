# README------------
# Extract annual/seasonal acres enrolled in Bird Returns program

# PACKAGES
library(tidyverse)
library(sf)

# INPUT DATA
brfields <- 'data/CONFIDENTIAL_br_fieldsMaster'

# OUTPUT DATA
br_enrollment <- 'data/BR_fields.csv'
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
  mutate(dates = gsub('Feb 1-', 'Feb 1 -', dates))

write_csv(dat, here::here(br_enrollment))


# SEASONAL TOTALS---------
# summarize total acreage enrolled by year and season

dat %>%
  group_by(year, season, dates, opt) %>%
  summarize(GIS_Ac = sum(GIS_Ac)) %>% 
  print(n = 37)

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
                         dates == 'Sep 2 - Sep 15' ~ 'Sep2A', #treat as the same
                         dates == 'Sep 1 - Sep 28' ~ 'Sep4A',
                         dates == 'Sep 15 - Sep 28' ~ 'Sep2B',
                         dates == 'Sep 16 - Sep 30' ~ 'Sep2B', #treat as the same
                         dates == 'Sep 17 - Oct 14' ~ 'Sep4B',
                         dates == 'Oct 1 - Oct 14' ~ 'Oct2A',
                         dates == 'Oct 4 - Oct 17' ~ 'Oct2A', #treat as the same
                         dates == 'Oct 1 - Oct 28' ~ 'Oct4A',
                         dates == 'Oct 15 - Oct 28' ~ 'Oct2B',
                         dates == 'Oct 18 - Oct 31' ~ 'Oct2B', #treat as the same
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
  select(-season, -dates, -opt, -program) %>%
  gather(jan_a:oct_b, key = 'timing', value = 'value') %>%
  mutate(timing = factor(timing, levels = c('jan_a', 'jan_b', 'feb_a', 'feb_b',
                                            'mar_a', 'mar_b', 'apr_a', 'aug_b',
                                            'sep_a', 'sep_b', 'oct_a', 'oct_b'))) %>%
  filter(value == 1) %>%
  group_by(year, timing) %>%
  summarize(GIS_Ac = sum(GIS_Ac)) %>%
  spread(key = timing, value = GIS_Ac, fill = 0)

write_csv(sdat, here::here(br_totals)) # Note: still missing some with no dates




dat %>% 
  ## FOR NOW, FOR CODE DEVELOPMENT PURPOSES, DROP MISSING DATES:
  filter(!is.na(dates)) %>%
  separate(dates, into = c('start', 'end'), sep = ' - ') %>%
  mutate(date_start = as.Date(paste(year, start), format = '%Y %b %d'),
         date_end = as.Date(paste(year, end), format = '%Y %b %d'),
         length = difftime(date_end, date_start, units = 'days'),
         length = as.numeric(length)) %>%
  select(polygonID:season, date_start, length) %>%
  spread(key = 'date_start', value = 'length')




sdat %>% filter(season == 'winter')
sdat %>% filter(season == 'spring') %>% arrange(opt)
sdat %>% filter(season == 'fall') %>% arrange(opt)

  
sdat <- std %>%
  group_by(year, season, opt) %>%
  summarize(Report_Ac = sum(Report_Ac),
            GIS_Ac = sum(GIS_Ac)) %>%
  filter(year < 2018)
sdat %>% print(n = 30)
sdat %>% filter(season == 'fall') %>% arrange(opt)
