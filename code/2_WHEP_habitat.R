# README--------------
# Compile time series of habitat available from fields enrolled 
# in WHEP programs. Distribute availability of fields across possible date
# ranges of each program.
# Still to do: add in estimates of habitat accessible (based on rice depth curves)

# PACKAGES
library(tidyverse)


# INPUT DATA
whepdata <- 'data/WHEP_summary.xlsx'
depthcurves <- 'data/cvjv_orig/depth_curves.csv'

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

## - WHEP_fall: stagger open water evenly over period 1 July - 15 Sep, 
##    with 2 weeks of "shallow" flooding + 2 week drawdown = 1 month of flooding
##    for each acre, beginning as late as 2 Sept (yday 64) to finish by 15 Sept
##    and start drawdown
##   -- assume 100% accessible because supposed to be "shallow" flooding?
##    Note: no data for fall 2013 (because in FY2013)

ts_fall <- expand.grid(yday = c(1:319), bioyear = c(2013:2016)) %>%
  mutate(habitat = 'whep_fall') %>%
  as.tibble() %>%
  # add total acres enrolled for reference
  left_join(tdat %>% filter(practice == 'WHEP_fall') %>% select(bioyear, ha)) %>%
  filter(!is.na(ha)) %>%
  #stagger addition evenly over 64 days of program, and returns begin after 1 month
  mutate(added = case_when(yday <= 64 ~ ha/64, 
                           TRUE ~ 0),
         returned = case_when(yday > 28 & yday <= 92 ~ ha/64,
                              TRUE ~ 0)) %>%
  group_by(bioyear) %>%
  mutate(lagreturn = lag(returned, 1),
         lagreturn = replace_na(lagreturn, 0),
         available = cumsum(added) - cumsum(lagreturn),
         accessible = available) %>%
  select(-lagreturn)

ggplot(ts_fall %>% select(-accessible) %>% 
         gather(added:available, key = 'var', value = 'value'), 
       aes(yday, value, color = var)) + geom_line() + 
  facet_wrap(~bioyear, nrow = 3) + 
  scale_x_continuous(breaks = c(1, 32, 63, 93), 
                     labels = c('Jul', 'Aug', 'Sep', 'Oct'),
                     limits = c(0, 93)) + ylab('ha')
##--> these are such tiny areas, consider excluding from analysis

## - WHEP VARIABLE DRAWDOWN: 
##     -- distribute flood-up linearly over the first ~2 weeks of Nov 
##          (supposed to be flooded up 1 Nov, but often not on time?),
##     -- distribute draw-down with 1/4 of area starting on each of
##          1 Feb (yday 215), 8 Feb, 15 Feb, and 21 Feb, each taking ~ 2 weeks 
##          to drain, so 1/4 returned on 15 Feb, 22 Feb, 1 Mar, 8 Mar
##  [[From Sesser et al. 2018: "Fields often remained puddled and saturated up to 
##    2 weeks after the water was allowed to drain from the field."]]
##     -- adjust original CVJV depth curves to apply to these timings

## original CVJV depth curves compared to timing of variable drawdown practice:
read_csv(here::here(depthcurves)) %>% filter(habitat == 'rice') %>%
  mutate(fit = case_when(yday<63 ~ NA_real_,
                         TRUE ~ fit)) %>%
  ggplot(aes(yday, fit, ymin = lcl, ymax = ucl)) + 
  geom_ribbon(fill = 'gray80') + geom_line() + xlab(NULL) + ylab('proportion <4"') +
  scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                     labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                'Feb', 'Mar', 'Apr', 'May'),
                     expand = c(0, 0)) +
  geom_vline(aes(xintercept = 124), col = 'red') + #start date of Vardd
  geom_vline(aes(xintercept = 216), col = 'red') + #start date of drawdown
  geom_vline(aes(xintercept = 216 + 43), col = 'blue') #presumed end date
##--> can assume full depth at start (i.e. no inverts accessible yet) and apply 
##     same tail in spring?

ts_vardd <- expand.grid(yday = c(1:319), bioyear = c(2013:2016)) %>%
  as.tibble() %>%
  left_join(tdat %>% filter(practice == 'WHEP_vardd') %>% select(bioyear, ha)) %>%
  mutate(habitat = 'whep_vardd',
         added = case_when(yday == 124 ~ ha, #all added on Nov 1
                           TRUE ~ 0),
         returned = case_when(yday == 215 + 14 ~ ha/4, #1/4 gone 2 weeks after boards pulled
                              yday == 215 + 21 ~ ha/4,
                              yday == 215 + 28 ~ ha/4,
                              yday == 215 + 35 ~ ha/4,
                              TRUE ~ 0)) %>%
  mutate(lagreturn = lag(returned, 1),
         lagreturn = replace_na(lagreturn, 0),
         available = cumsum(added) - cumsum(lagreturn)) %>%
  select(-lagreturn) %>%
  left_join(read_csv(here::here(depthcurves), col_types = cols()) %>% 
              filter(habitat == 'rice') %>%
              select(yday, prop.accessible = fit),
            by = c('yday')) %>%
  mutate(accessible = available * prop.accessible,
         prop.accessible = NULL)

ggplot(ts_vardd, aes(yday, added)) + geom_line() + 
  geom_line(aes(y = returned), col = 'red') +
  geom_line(aes(y = available), col = 'blue') +
  geom_line(aes(y = accessible), col = 'purple') +
  facet_wrap(~bioyear)


## - combined:
ts <- bind_rows(ts_fall, ts_vardd) %>%
  ungroup() %>%
  mutate(group = recode(bioyear, 
                        '2013' = '2013-14',
                        '2014' = '2014-15',
                        '2015' = '2015-16',
                        '2016' = '2016-17')) %>%
  complete(group, habitat, yday = 1:319, 
           fill = list(available = 0, accessible = 0, added = 0, returned = 0)) %>%
  mutate(prop.accessible = case_when(habitat == 'whep_fall' ~ 1,
                                     habitat == 'whep_vardd' ~ accessible / available),
         prop.accessible = case_when(is.nan(prop.accessible) ~ 1,
                                     TRUE ~ prop.accessible)) %>%
  select(habitat, group, yday, available, accessible, added, returned, prop.accessible)

ggplot(ts, aes(yday, available, color = habitat)) + geom_line() + 
  facet_wrap(~group)
ggplot(ts, aes(yday, accessible, color = habitat)) + geom_line() + 
  facet_wrap(~group)
ggplot(ts, aes(yday, prop.accessible, color = habitat)) + geom_line() + 
  facet_wrap(~group)

write_csv(ts, here::here(whep_ts))
