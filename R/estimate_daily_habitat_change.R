##' @title Estimate daily habitat changes
##' @param basepath path to CSV containing estimates of total area (ha) of each
##'   land cover type
##' @param depthpath path to CSV containing estimates of daily proportion
##'   suitable depth by land cover type
##' @param floodcurves list object containing annual estimates of daily
##'   proportion open water by land cover type
##' @param time field name for the day of year (time interval) in floodcurves
##'   (see bioenergmod::calculate_habitat_change)
##' @param value field name for the daily proportion open water estimates in
##'   floodcurves (see bioenergmod::calculate_habitat_change)
##' @param wetsplit default TRUE; whether or not wetland flood curves data
##'   should be split by prop.perm (see bioenergmod::calculate_habitat_change)
##' @param ... tibble objects containing time series data on acres enrolled in
##'   incentive programs (to be appended to background baseline estimates of
##'   habitat availability)
##' @description Functions for estimating the total open water and accessible
##'   open water in all land cover types on each day of the nonbreeding season
##'   from updated flooding curves and original CVJV depth curves (i.e.,
##'   proportion of open water in each land cover type that is of suitable depth
##'   for shorebird foraging habitat), plus data from Bird Returns and WHEP
##'   programs

estimate_habitat_change <- function(basepath, depthpath, floodcurves, 
                                    time = 'yday', value = 'fit',  
                                    wetsplit = TRUE) {
  base <- read_csv(basepath, col_types = cols()) %>% 
    filter(!habitat %in% c('corn', 'seas', 'perm')) %>% 
    mutate(habitat = recode(habitat, corn_north = 'corn')) %>% 
    list()
  
  depthcurves <- read_csv(depthpath, col_types = cols()) %>%
    filter(habitat != 'corn') %>%
    mutate(habitat = recode(habitat, corn_north = 'corn'))

  floodcurves <- floodcurves %>% split(.$group)
  
  change <- map2(base, floodcurves, bioenergmod::calculate_habitat_change,
                 time = 'yday', value = 'fit',
                 accessible = depthcurves, wetsplit = TRUE)
  names(change) = names(floodcurves)
  
  ## assume rice, corn, and other crops are not accessible prior to Sept 1
  ## (day 63)
  change$`2013-14`$added[1:62, c('rice', 'corn', 'other')] = 0
  change$`2014-15`$added[1:62, c('rice', 'corn', 'other')] = 0
  change$`2015-16`$added[1:62, c('rice', 'corn', 'other')] = 0
  change$`2016-17`$added[1:62, c('rice', 'corn', 'other')] = 0
  change$`2013-14`$returned[1:62, c('rice', 'corn', 'other')] = 0
  change$`2014-15`$returned[1:62, c('rice', 'corn', 'other')] = 0
  change$`2015-16`$returned[1:62, c('rice', 'corn', 'other')] = 0
  change$`2016-17`$returned[1:62, c('rice', 'corn', 'other')] = 0
  change$`2013-14`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  change$`2014-15`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  change$`2015-16`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  change$`2016-17`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  change$`2013-14`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  change$`2014-15`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  change$`2015-16`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  change$`2016-17`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  
  return(change)
}

append_incentives <- function(list, ...) {
  #append incentive data
  incentives <- bind_rows(...) %>% 
    rename(time = yday, openwater = available) %>% 
    pivot_longer(openwater:prop.accessible) %>% 
    mutate(name = factor(name, levels = c('openwater', 'added', 'returned', 
                                          'accessible', 'prop.accessible'))) %>%
    pivot_wider(names_from = 'habitat', values_from = 'value', 
                values_fill = list(value = 0)) %>% 
    arrange(group, name, time) %>% 
    split(.$group) %>% 
    map(~split(., .$name))
  
  map2(list, incentives, .f = function(a, b) {
    map2(a, 
         b %>% map(~ .x %>% select(-name, -group)),
         full_join, by = 'time')
    })  
}

compile_habitat_availability <- function(list) {
  bind_rows(
    map_dfr(list, ~.x[['openwater']] %>% mutate(watertype = 'open'), 
            .id = 'group'),
    map_dfr(list, ~.x[['accessible']] %>% mutate(watertype = 'accessible'), 
            .id = 'group')) %>% 
    mutate(incentives = br_fall + br_spring + whep_fall + whep_vardd,
           wetlands = seas + perm) %>%
    select(group, time, watertype, wetlands, rice, corn, other, incentives, 
           br_fall, br_spring, whep_fall, whep_vardd)
}