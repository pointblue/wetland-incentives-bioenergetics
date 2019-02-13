# README--------------
# Stub for developing WHEP habitat availability and accessibility estimates

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
ts <- data.frame(yday = c(1:92),
           bioyear = 2014,
           added = c(rep(138/64, 64), rep(0, 28)),
           returned = c(rep(0, 14), cumsum(rep(138/64/14, 14)), 
                        rep(138/64, 64 - 15), 
                        rev(cumsum(rep(138/64/14, 14))), 0)) %>%
  mutate(lagreturn = lag(returned, 1),
         lagreturn = replace_na(lagreturn, 0),
         available = cumsum(added) - cumsum(lagreturn)) %>%
  select(-lagreturn)


ggplot(ts, aes(yday, added)) + geom_line() + 
  geom_line(aes(y = returned), col = 'red') +
  geom_line(aes(y = available), col = 'blue')





df <- expand.grid(yday = c(1:320),
                  bioyear = c(2013:2016))
ts <- pmap_dfr(list(df$yday, df$bioyear),
               function(y, byear) {
                 res <- mdat %>% 
                   filter(ystart <= y & yend >= y & bioyear == byear) %>%
                   mutate(group = case_when(ystart == y ~ 'added',
                                            yend == y ~ 'returned',
                                            TRUE ~ 'available')) %>%
                   group_by(group) %>%
                   summarize(ha = sum(GIS_Ac) / 2.47105) %>%
                   mutate(yday = y, bioyear = byear)
               }
) 
