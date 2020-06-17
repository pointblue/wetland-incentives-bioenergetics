##' @title Get Bird Returns data
##' @param shp path to shapefile containing Bird Returns metadata
##' @param df tibble
##' @param drawdown average number of days beyond the end of official Bird
##'   Returns enrollment period for which fields are assumed to still contain
##'   water
##' @param rates path to csv containing fields "date" (arbitrary year) and
##'   corresponding estimated "compliance_rate" (proportion)
##' @description Functions for compiling time series of habitat available and
##'   accessible from fields enrolled in Bird Returns program. Assume fields are
##'   100% available (open water) and accessible (suitable shorebird depth)
##'   during enrollment.


# remove individual names and simplify fields
anonymize_BR_spatialdat <- function(shp) {
  st_read(shp, quiet = TRUE) %>%
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
    select(polygonID, UniqueID, GIS_Ac, d_sprg_2014:last_col()) 
}

# clean up data
cleanup_BR_dat <- function(shp) {
  shp %>% st_set_geometry(NULL) %>% 
    pivot_longer(d_sprg_2014:d_fa18_dat, names_to = 'var', values_to = 'value') %>%
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
    pivot_wider(names_from = 'var', values_from = 'value') %>%
    filter(program == 'enrolled') %>% #INCLUDE ONLY ENROLLED FIELDS
    arrange(year, season, polygonID) %>%
    mutate(dates = gsub('Feb 1-', 'Feb 1 -', dates),
           opt = gsub('Aug A', 'Aug B', opt),
           dates = case_when(opt == 'Aug B' & is.na(dates) ~ 'Aug 15 - Aug 28',
                             TRUE ~ dates)) %>%  #fix typo
    # believe not actually enrolled after all
    filter(!(UniqueID %in% c('yu2472', 'yu2416', 'yu1668', 'su954', 'yu2473') & 
               season == 'fall' & year == 2014)) %>%
    # control fields (for surveys, not flooded)
    filter(!(UniqueID %in% c('yo3453', 'yo3454', 'yo3455', 'yo3464') & 
               season == 'fall' & year == 2014)) %>%
    # add dates
    mutate(dates = case_when(UniqueID %in% c('yu2465', 'yu2469', 'yu1670', 
                                             'yu1669', 'yo3463', 'yu2463', 
                                             'yo3447') & 
                               season == 'fall' & year == '2014' ~ 
                               'Oct 18 - Oct 31',
                             UniqueID == 'yo3447' & season == 'spring' & 
                               year == '2014' ~ 'Mar 10 - Apr 4',
                             TRUE ~ dates),
           opt = case_when(UniqueID %in% c('yu2465', 'yu2469', 'yu1670', 
                                           'yu1669', 'yo3463', 'yu2463', 
                                           'yo3447') & 
                             season == 'fall' & year == '2014' ~ 'Oct B',
                           UniqueID == 'yo3447' & season == 'spring' & 
                             year == '2014' ~ 'Mar B',
                           TRUE ~ opt))
}

# get start and end dates of enrollment for each field in each season/year
# from date range text
get_BR_dates <- function(df) {
  df %>% 
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
  
}


# for each day of each bioyear, identify total area of fields just added that 
#   day ('added'), currently flooded ('available'), or went dry that day 
#   ('returned'); 
#   --> assuming it takes 2 weeks from end of contract for field to fully dry
get_BR_timeseries <- function(df, drawdown = 14) {
  scaffold <- expand.grid(yday = c(1:319), bioyear = c(2013:2016))
  pmap_dfr(list(scaffold$yday, scaffold$bioyear),
           function(y, byear) {
             df %>% 
               # for each day in scaffold, find fields that currently have water 
               # (ystart on or before today, and yend + drawdown is on or after today)
               filter(ystart <= y & (yend + drawdown) >= y & bioyear == byear) %>%
               # if ystart = today ~ newly added
               # if yend + drawdown = today ~ just returned
               # otherwise, currently "available" habitat
               mutate(group = case_when(ystart == y ~ 'added',
                                        yend + drawdown == y ~ 'returned',
                                        TRUE ~ 'available')) %>%
               group_by(group) %>%
               summarize(ha = sum(GIS_Ac) / 2.47105, .groups = 'drop') %>%
               mutate(yday = y, bioyear = byear)}
  ) %>% 
    complete(bioyear, yday = 1:319,
             fill = list(group = 'available', ha = 0)) %>%
    mutate(habitat = ifelse(yday < 185, 'br_fall', 'br_spring')) %>%
    pivot_wider(names_from = group, values_from = ha, values_fill = list(ha = 0)) %>%
    arrange(bioyear, yday, habitat) %>%
    # if returned or added, also consider it available that day (additions count
    # from start of day; returns go away at end of day)
    mutate(available = available + added + returned,
           group = recode(bioyear,
                          '2013' = '2013-14',
                          '2014' = '2014-15',
                          '2015' = '2015-16',
                          '2016' = '2016-17')) 
}

# COMPLIANCE assumptions: depth stake data shows not quite 100% compliance with
# requirement to stay <10cm deep estimated generalized depth curves from Greg
# Golet:
# fall: 70% compliance up to Oct 14, 80% Oct 14-31, 90% thereafter
#   -->assume starts increasing to 80% on Oct 1
# spring: 70% compliance Feb1-14, 80% Feb 15-21, 90% Feb 22-28, 100% thereafter
#   -->assume gradual increases from 70% on Feb1 to 80% on Feb15, etc.

estimate_BR_compliance <- function(df, rates) {
  compliance <- read_csv(rates, col_types = cols()) %>% 
    mutate(yday = as.numeric(format(date, '%j')),
           yday = case_when(date >= '2017-01-01' ~ yday + 184,
                            date < '2017-01-01' ~ yday - 181)) %>%
    select(yday, prop.accessible = compliance_rate)
  
  interpolated <- bind_rows(
    # flat linear interpolation between end points with same prop.accessible
    approx(compliance$yday, compliance$prop.accessible,
           xout = c(46:93, 124:153, 185:216, 244:304)), 
    # spline over fall where compliance is increasing
    spline(compliance$yday, compliance$prop.accessible,
           method = 'natural', xmin = 93, xmax = 124, n = 32), 
    # spline over spring where compliance is increasing
    spline(compliance$yday, compliance$prop.accessible,
           method = 'natural', xmin = 216, xmax = 244, n = 29)) %>%
    distinct() %>% # remove duplicates at days 93 and 124
    arrange(x) %>% 
    rename(yday = x, prop.accessible = y)
  
  left_join(df, interpolated, by = 'yday') %>% 
    #fill missing values outside of BR program:
    mutate(prop.accessible = replace_na(prop.accessible, 1),
           accessible = available * prop.accessible) %>% 
    select(habitat, group, yday, available, accessible, added, returned, 
           prop.accessible)
}
