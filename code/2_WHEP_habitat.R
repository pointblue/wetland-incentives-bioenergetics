# README--------------
# Compile time series of habitat available from fields enrolled 
# in WHEP programs. Distribute availability of fields across possible date
# ranges of each program.
# Still to do: add in estimates of habitat accessible (based on rice depth curves)

# PACKAGES
library(tidyverse)


# INPUT DATA
whepdata <- 'data/WHEP_summary.xlsx'

# OUTPUT DATA
whep_ts <- 'data/WHEP_timeseries.csv'
whep_totals <- 'data/WHEP_totals.csv'


# ANNUAL TOTALS----------------
dat <- readxl::read_xlsx(here::here(whepdata))[9:12,] %>%
  gather(FY2014:FY2017, key = 'fiscalyear', value = 'acres') %>%
  mutate(ha = as.numeric(acres) / 2.47105,
         practice = case_when(`fiscal year` == 'Fall flooding (2 weeks in July,Aug,or Sep)' ~ 'WHEP_fall',
                              `fiscal year` == 'Boards in water control sturctures (Nov-Jan)' ~ 'WHEP_boardsin',
                              `fiscal year` == 'Variable Drawdown (flooded ~Nov - Feb1, then dd thru Feb)' ~ 'WHEP_vardd',
                              TRUE ~ 'WHEP_total')) %>%
  select(practice, fiscalyear, ha) %>%
  filter(practice != 'WHEP_total') %>%
  mutate(fiscalyear = as.numeric(substr(fiscalyear, 3, 6)),
         bioyear = case_when(practice != 'WHEP_fall' ~ fiscalyear - 1,
                             TRUE ~ fiscalyear)) %>%
  filter(bioyear <= 2016) %>%
  arrange(practice, bioyear) %>%
  mutate(label = recode(bioyear, 
                        '2013' = '2013-14',
                        '2014' = '2014-15',
                        '2015' = '2015-16',
                        '2016' = '2016-17'))
  
tdat <- dat %>%
  # vardd = flood 1 Nov - 1 Feb, plus gradual drawdown through Feb;
  # boardsin = boards in (passive rain capture) 1 Nov - 1 Feb
  mutate(nov_a = case_when(practice == 'WHEP_vardd' ~ 1,
                           practice == 'WHEP_boardsin' ~ 2,
                           TRUE ~ 0),
         nov_b = case_when(practice == 'WHEP_vardd' ~ 1,
                           practice == 'WHEP_boardsin' ~ 2,
                           TRUE ~ 0),
         dec_a = case_when(practice == 'WHEP_vardd' ~ 1,
                           practice == 'WHEP_boardsin' ~ 2,
                           TRUE ~ 0),
         dec_b = case_when(practice == 'WHEP_vardd' ~ 1,
                           practice == 'WHEP_boardsin' ~ 2,
                           TRUE ~ 0),
         jan_a = case_when(practice == 'WHEP_vardd' ~ 1,
                           practice == 'WHEP_boardsin' ~ 2,
                           TRUE ~ 0),
         jan_b = case_when(practice == 'WHEP_vardd' ~ 1,
                           practice == 'WHEP_boardsin' ~ 2,
                           TRUE ~ 0),
         feb_a = case_when(practice == 'WHEP_vardd' ~ 2,
                           practice == 'WHEP_boardsin' ~ 0,
                           TRUE ~ 0),
         feb_b = case_when(practice == 'WHEP_vardd' ~ 2,
                           practice == 'WHEP_boardsin' ~ 0,
                           TRUE ~ 0),
         mar_a = case_when(practice == 'WHEP_vardd' ~ 0,
                           practice == 'WHEP_boardsin' ~ 0,
                           TRUE ~ 0),
         mar_b = case_when(practice == 'WHEP_vardd' ~ 0,
                           practice == 'WHEP_boardsin' ~ 0,
                           TRUE ~ 0),
         apr_a = case_when(practice == 'WHEP_vardd' ~ 0,
                           practice == 'WHEP_boardsin' ~ 0,
                           TRUE ~ 0),
         # fall practice = 2 weeks of flooding any time between 1 July - 15 Sep, 
         #  plus 2 weeks of gradual drawdown
         jul_a = case_when(practice == 'WHEP_fall' ~ 2,
                           TRUE ~ 0),
         jul_b = case_when(practice == 'WHEP_fall' ~ 2,
                           TRUE ~ 0),
         aug_a = case_when(practice == 'WHEP_fall' ~ 2,
                           TRUE ~ 0),
         aug_b = case_when(practice == 'WHEP_fall' ~ 2,
                           TRUE ~ 0),
         sep_a = case_when(practice == 'WHEP_fall' ~ 2,
                           TRUE ~ 0),
         sep_b = case_when(practice == 'WHEP_fall' ~ 2,
                           TRUE ~ 0))

write_csv(tdat, here::here(whep_totals))

# TIME SERIES----------------

mdat <- dat %>% 
  mutate(start = case_when(practice != 'WHEP_fall' ~ as.Date(paste0(bioyear, '-11-01')),
                           TRUE ~ as.Date(paste0(fiscalyear, '-07-01'))),
         end = case_when(practice == 'WHEP_boardsin' ~ as.Date(paste0(fiscalyear, '-02-01')),
                         practice == 'WHEP_vardd' ~ as.Date(paste0(fiscalyear, '-02-28')),
                         TRUE ~ as.Date(paste0(fiscalyear, '-09-30')))) %>%
  # calculate day of (calendar) year
  mutate(ystart = as.numeric(format(start, '%j')),
         yend = as.numeric(format(end, '%j'))) %>%
  # adjust for bioyear (Jul to Jun) -- no date ranges span Jun 30-Jul 1
  mutate(startm = as.numeric(format(start, '%m')),
         endm = as.numeric(format(end, '%m')),
         ystart = case_when(startm <= 6 ~ ystart + 184,
                            startm >= 7 ~ ystart - 181),
         yend = case_when(endm <= 6 ~ yend + 184,
                          endm >= 7 ~ yend - 181)) %>%
  # fix leap year
  mutate(ystart = case_when(bioyear == 2016 ~ ystart - 1,
                            TRUE ~ ystart),
         yend = case_when(bioyear == 2016 & yend < 184 ~ yend - 1,
                          TRUE ~ yend))

## rather than applying a simple assumption as for Bird Returns,
##  estimate a proportion open water under each practice

## - WHEP_fall: distribute evenly over period 1 July - 15 Sep + 2 week drawdown
##    The latest date new acres could be added is Sep 2 (yday 64) to allow 2 weeks of 
##    flooding by Sept 15
##    Note: no data for fall 2013 (because in FY2013)

ts_fall <- expand.grid(yday = c(1:92), bioyear = c(2013:2016)) %>%
  as.tibble() %>%
  left_join(tdat %>% filter(practice == 'WHEP_fall') %>% select(bioyear, ha)) %>%
  filter(!is.na(ha)) %>%
  mutate(added = case_when(yday <= 64 ~ ha/64,
                           TRUE ~ 0),
         returned = case_when(yday > 14 & yday <= 28 ~ ha/64/14,
                              yday > 28 ~ ha/64,
                              # yday > 77 & yday <92 ~ ha/64/14,
                              # yday > 28 & yday <=77 ~ cumsum(ha/64/14),
                              # yday > 77 ~ rev(cumsum(rep(ha/64/14))),
                              TRUE ~ 0)) %>%
  group_by(bioyear) %>%
  mutate(rampup = case_when(yday > 14 & yday <= 28 ~ returned,
                            TRUE ~ 0),
         rampup = cumsum(rampup),
         returned = case_when(yday > 14 & yday <= 28 ~ rampup,
                              TRUE ~ returned),
         rampup = NULL) %>%
  mutate(taper = case_when(yday > 78 ~ ha/64/14,
                           TRUE ~ 0),
         taper = cumsum(taper),
         returned = case_when(yday > 77 ~ returned - taper,
                           TRUE ~ returned),
         taper = NULL) %>%
  mutate(lagreturn = lag(returned, 1),
         lagreturn = replace_na(lagreturn, 0),
         available = cumsum(added) - cumsum(lagreturn)) %>%
  select(-lagreturn) %>%
  mutate(landcover = 'whep_fall')

ggplot(ts_fall, aes(yday, added)) + geom_line() + 
  geom_line(aes(y = returned), col = 'red') +
  geom_line(aes(y = available), col = 'blue') +
  facet_wrap(~bioyear)


## - WHEP VARIABLE DRAWDOWN: distribute flood-up over the first ~2 weeks of Nov,
##     and drawdown over 4 weeks of Feb

ts_vardd <- expand.grid(yday = c(124:243), bioyear = c(2013:2016)) %>%
  as.tibble() %>%
  left_join(tdat %>% filter(practice == 'WHEP_vardd') %>% select(bioyear, ha)) %>%
  mutate(added = case_when(yday < 124 + 14 ~ ha / 14,
                           TRUE ~ 0),
         returned = case_when(yday > 215 & yday <= 215 + 7 ~ ha/4/7 + ha/4/14 + ha/4/21 + ha/4/28,
                              yday > 215 + 7 & yday <= 215 + 14 ~ ha/4/14 + ha/4/21 + ha/4/28,
                              yday > 215 + 14 & yday <= 215 + 21 ~ ha/4/21 + ha/4/28,
                              yday > 215 + 21 ~ ha/4/28,
                              TRUE ~ 0)) %>%
  mutate(lagreturn = lag(returned, 1),
         lagreturn = replace_na(lagreturn, 0),
         available = cumsum(added) - cumsum(lagreturn)) %>%
  select(-lagreturn) %>%
  mutate(landcover = 'whep_vardd')

ggplot(ts_vardd, aes(yday, added)) + geom_line() + 
  geom_line(aes(y = returned), col = 'red') +
  geom_line(aes(y = available), col = 'blue') +
  facet_wrap(~bioyear)


## - combined:
ts <- bind_rows(ts_fall, ts_vardd) %>%
  complete(landcover, bioyear, yday = 1:320, fill = list(available = 0, added = 0, returned = 0)) 

ggplot(ts, aes(yday, added, linetype = landcover)) + geom_line() + 
  geom_line(aes(y = returned), col = 'red') +
  geom_line(aes(y = available), col = 'blue') +
  facet_wrap(~bioyear)

write_csv(ts, here::here(whep_ts))
