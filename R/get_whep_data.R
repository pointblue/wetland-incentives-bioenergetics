##' @title Get WHEP data
##' @param path path to xlsx containing summary of WHEP data by year
##' @param cols columns in xlsx file to read in
##' @param drawdown average number of days beyond the end of official incentive
##'   program enrollment period for which fields are assumed to still contain
##'   water
##' @param rates path to CSV containing estimated proportion of open water that
##'   is of suitable depth, by date
##' @description Functions for compiling time series of habitat available from
##'   fields enrolled in WHEP programs.

# ANNUAL TOTALS----------------
get_WHEP_data <- function(path, cols) {
  df <- readxl::read_xlsx(path)[cols,] %>%
    pivot_longer(FY2014:FY2017, names_to = 'fiscalyear', values_to = 'acres') %>%
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
    arrange(practice, bioyear)
}

get_WHEP_timeseries <- function(df, drawdown = 14) {
  scaffold <- expand.grid(yday = c(1:319), bioyear = c(2013:2016)) %>% 
    as_tibble()
  
  # fall flooding: stagger open water evenly over period 1 July - 15 Sep, 
  #   assume compliance with 2 weeks of "shallow" flooding + drawdown
  # - begins as late as 2 Sept (yday 64) to finish by 15 Sept (yday 77) and start drawdown
  # - assume 90% accessible because supposed to be "shallow" flooding?
  # Note: for 2013 - flooding was for 4 weeks! (so begin as late as yday 50?)
  
  fall <- scaffold %>% 
    # add total annual acres enrolled for reference
    left_join(df %>% filter(practice == 'WHEP_fall') %>% select(bioyear, ha),
              by = 'bioyear') %>%
    #stagger addition evenly over 64 days of program, and returns begin after 1 month
    mutate(ha = replace_na(ha, 0),
           #stagger addition evenly over 64 days of possible program start dates
           #(or 50 days for 2013)
           added = case_when(bioyear == 2013 & yday <= 50 ~ ha/50, 
                             bioyear != 2013 & yday <= 64 ~ ha/64,
                             TRUE ~ 0),
           #returns begin after 2 wks + drawdown (4 wks in 2013); returns end 2
           #wks after day 64 + drawdown (or 4 wks + drawdown in 2013)
           returned = case_when(bioyear == 2013 & yday > (28 + drawdown) &
                                  yday <= (50 + 28 + drawdown) ~ ha/50,
                                bioyear != 2013 & yday > (14 + drawdown) & 
                                  yday <= (64 + 14 + drawdown) ~ ha/64,
                                TRUE ~ 0)) %>%
    # from daily amount added and returned, calculate a daily amount available:
    #  total cumulative added minus total cumulative returned by the next day
    group_by(bioyear) %>%
    mutate(lagreturn = lag(returned, 1),
           lagreturn = replace_na(lagreturn, 0),
           available = cumsum(added) - cumsum(lagreturn),
           habitat = 'whep_fall') %>%
    ungroup() %>% 
    select(-lagreturn, -ha)
  
  # Variable drawdown 
  # from Kristin: annual variability in start dates/water curtailments/compliance
  # best estimates:
  #  2013-14: half of acreage enrolled is flooded 1 Nov, half 1 Dec
  #  2014-15: half start 1 Dec, half 15 Dec
  #  2015-16: half start 1 Nov, half 15 Nov
  #  2016-17: same as 2015-16
  #     -- distribute flood-up linearly over the first ~2 weeks of Nov
  #          (supposed to be flooded up 1 Nov, but often not on time?),
  #     -- distribute draw-down with 1/4 of area starting on each of
  #          1 Feb (yday 215), 8 Feb, 15 Feb, and 21 Feb, each taking ~ 2 weeks
  #          to drain, so 1/4 returned on 15 Feb, 22 Feb, 1 Mar, 8 Mar
  #  [[From Sesser et al. 2018: "Fields often remained puddled and saturated up to
  #    2 weeks after the water was allowed to drain from the field."]]
  #     -- adjust original CVJV depth curves to apply to these timings
  
  vardd <- scaffold %>% 
    left_join(df %>% filter(practice == 'WHEP_vardd') %>% select(bioyear, ha),
              by = 'bioyear') %>%
    mutate(added = case_when(bioyear %in% c(2013, 2015, 2016) & yday == 124 ~ 
                               ha/2, #1st half added on Nov 1
                             bioyear %in% c(2015, 2016) & yday == 138 ~ 
                               ha/2, #2nd half on Nov 15,
                             bioyear %in% c(2013, 2014) & yday == 154 ~ 
                               ha/2, #2nd half for 2013, 1st half in 2014
                             bioyear == 2014 & yday == 168 ~ 
                               ha/2, #2nd half for 2014
                             TRUE ~ 0),
           #boards pulled on 1/4 of acres each week in Feb; dry after drawdown period
           returned = case_when(yday == 215 + drawdown ~ ha/4, 
                                yday == 215 + drawdown + 7 ~ ha/4,
                                yday == 215 + drawdown + 14 ~ ha/4,
                                yday == 215 + drawdown + 21 ~ ha/4,
                                TRUE ~ 0)) %>%
    # from daily amount added and returned, calculate a daily amount available:
    #  total cumulative added minus total cumulative returned by the next day
    group_by(bioyear) %>%
    mutate(lagreturn = lag(returned, 1),
           lagreturn = replace_na(lagreturn, 0),
           available = cumsum(added) - cumsum(lagreturn),
           habitat = 'whep_vardd') %>%
    ungroup() %>% 
    select(-lagreturn, -ha)
  
  bind_rows(fall, vardd) %>%
    complete(bioyear, habitat, yday = 1:319,
             fill = list(available = 0, added = 0, returned = 0)) %>%
    select(habitat, bioyear, yday, available, added, returned) %>% 
    mutate(group = recode(bioyear, 
                          '2013' = '2013-14',
                          '2014' = '2014-15',
                          '2015' = '2015-16',
                          '2016' = '2016-17'))
}

estimate_WHEP_compliance <- function(df, rates) {
  # for whep_fall assume 90% compliance; for whep_vardd assume original rice
  # depth curves apply
  orig_curves <- read_csv(rates, col_types = cols()) %>% 
    filter(habitat == 'rice') %>% 
    select(yday, fit)
  
  df %>% 
    left_join(orig_curves, by = 'yday') %>% 
    mutate(prop.accessible = case_when(habitat == 'whep_fall' ~ 0.9,
                                       habitat == 'whep_vardd' ~ fit),
           accessible = available * prop.accessible) %>% 
    select(habitat, group, yday, available, accessible, added, returned, 
           prop.accessible)
}
