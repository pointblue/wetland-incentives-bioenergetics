##' @title Get landscape data
##' @param t vector containing list of 4-digit years for which NASS data should
##'   be requested
##' @param cvjv.match logical determining whether or not NASS data should be
##'   requested in a way to match quirks from the 2017 CVJV non-breeding
##'   shorebird technical manuscript (i.e. including mint - originally included
##'   in field crops, and "vegetable totals" which may double-count some of the
##'   vegetables included elsewhere)
##' @param proportions path to CSV containing the estimated proportion of
##'   statewide crop totals typically found within each of the CVJV basins
##' @param seas total estimated area of seasonal wetlands (hectares)
##' @param perm total estimated area of permanent/semi-permanent wetlands
##'   (hectares)
##' @description Functions to compile time series of total potential habitat
##'   available from crops and wetlands in the Central Valley
##' @details requires API from: https://quickstats.nass.usda.gov/api; store as
##'   NASS_TOKEN in .Renviron. The easiest way to edit is:
##'   usethis::edit_r_environ()


extract_nass = function(t, cvjv.match = FALSE) {
  res <- bind_rows(
    ## field crops
    nass_data(source_desc = 'SURVEY', 
              sector_desc = 'CROPS', 
              group_desc = 'FIELD CROPS', 
              class_desc = 'ALL CLASSES',
              prodn_practice_desc = 'ALL PRODUCTION PRACTICES',
              statisticcat_desc = 'AREA PLANTED',
              agg_level_desc = 'STATE',
              state_name = 'CALIFORNIA', 
              year = as.character(t), 
              reference_period_desc = 'YEAR',
              token = Sys.getenv('NASS_TOKEN')),
    ## plus beans
    nass_data(source_desc = 'SURVEY', 
              sector_desc = 'CROPS', 
              commodity_desc =  'BEANS', 
              # class_desc = 'DRY EDIBLE',
              prodn_practice_desc = 'ALL PRODUCTION PRACTICES',
              statisticcat_desc = 'AREA PLANTED',
              agg_level_desc = 'STATE',
              state_name = 'CALIFORNIA', 
              year = as.character(t), 
              reference_period_desc = 'YEAR',
              token = Sys.getenv('NASS_TOKEN')),
    # ## and alfalfa
    # nass_data(source_desc = 'SURVEY', 
    #           sector_desc = 'CROPS', 
    #           commodity_desc =  'HAY & HAYLAGE', 
    #           statisticcat_desc = 'AREA PLANTED',
    #           agg_level_desc = 'STATE',
    #           state_name = 'CALIFORNIA', 
    #           year = as.character(t), 
    #           reference_period_desc = 'YEAR',
    #           token = Sys.getenv('NASS_TOKEN' )),
    ## and vegetables
    if (t < 2016) {
      nass_data(source_desc = 'SURVEY', 
                sector_desc = 'CROPS', 
                group_desc = 'VEGETABLES', 
                commodity_desc = 'VEGETABLE TOTALS',
                class_desc = '34 MAJOR, VARIES BY SEASON',
                statisticcat_desc = 'AREA PLANTED',
                agg_level_desc = 'STATE',
                state_name = 'CALIFORNIA', 
                year = as.character(t), 
                reference_period_desc = 'YEAR',
                token = Sys.getenv('NASS_TOKEN'))
    } else if (t >= 2016) {
      nass_data(source_desc = 'SURVEY', 
                # sector_desc = 'CROPS', 
                # group_desc = 'VEGETABLES', 
                commodity_desc = 'VEGETABLE TOTALS',
                class_desc = 'ALL CLASSES',
                statisticcat_desc = 'AREA PLANTED',
                agg_level_desc = 'STATE',
                state_name = 'CALIFORNIA', 
                year = as.character(t), 
                reference_period_desc = 'YEAR',
                token = Sys.getenv('NASS_TOKEN')) 
    },
    ## and mint (included in field crops used by CVJV, keep for comparability)
    if (cvjv.match == TRUE & t > 2007) {
      nass_data(source_desc = 'SURVEY',
                sector_desc = 'CROPS',
                commodity_desc = 'MINT',
                class_desc = 'PEPPERMINT',
                prodn_practice_desc = 'ALL PRODUCTION PRACTICES',
                statisticcat_desc = 'AREA HARVESTED',
                agg_level_desc = 'STATE',
                state_name = 'CALIFORNIA',
                year = as.character(t),
                reference_period_desc = 'YEAR',
                token = Sys.getenv('NASS_TOKEN'))
    },
    ## and double-counting some vegetables by mistake, keep for comparability to CVJV
    if (cvjv.match == TRUE & t < 2016) {
      nass_data(source_desc = 'SURVEY', 
                sector_desc = 'CROPS', 
                group_desc = 'VEGETABLES', 
                commodity_desc = 'VEGETABLE TOTALS',
                class_desc = '10 MAJOR, VARIES BY SEASON',
                statisticcat_desc = 'AREA PLANTED',
                agg_level_desc = 'STATE',
                state_name = 'CALIFORNIA', 
                year = as.character(t), 
                reference_period_desc = 'YEAR',
                token = Sys.getenv('NASS_TOKEN'))
    } 
  )
}

# group into crop classes and summarize by year
summarize_cropclass <- function(df) {
  df %>% select(commodity_desc, year, acres = Value) %>%
    # # match some values that were different in original NASS data used for CVJV
    # mutate(acres = case_when(commodity_desc == 'VEGETABLE TOTALS' & year == 2014 & acres == 737300 ~ 750000, 
    #                          commodity_desc == 'COTTON' & year == 2010 & acres == 306000 ~ 303000,
    #                          commodity_desc == 'COTTON' & year == 2007 & acres == 455000 ~ 451000,
    #                          TRUE ~ acres)) %>%
    mutate(crop_class = case_when(commodity_desc %in% c('CORN', 'RICE') ~ tolower(commodity_desc),
                                  commodity_desc == 'VEGETABLE TOTALS' ~ 'rowcrop',
                                  commodity_desc %in% c('BARLEY', 'OATS', 'WHEAT') ~ 'grains',
                                  commodity_desc %in% c('BEANS', 'CHICKPEAS', 
                                                        'COTTON', 'MINT', 
                                                        'SAFFLOWER', 'SORGHUM', 
                                                        'SUGARBEETS', 'SUNFLOWER') ~ 'fieldcrop',
                                  TRUE ~ NA_character_),
           year = as.numeric(year)) %>%
    filter(!is.na(acres)) %>%
    group_by(crop_class, year) %>%
    summarize(acres = sum(acres), .groups = 'drop')
}

# allocate annual crop totals to basins (and therefore the CVJV primary focus area)
# based on average spatial distribution of crops in CA (same spatial distribution
# assumed for CVJV non-breeding shorebirds paper)

estimate_cvjv_croptotals <- function(df, proportions) {
  prop <- read_csv(proportions, col_types = cols()) %>%
    filter(TREND_CATE %in% c('Field Crop', 'Row Crop', 'Grains', 'Corn', 'Rice')) %>%
    rename(basin = Primary_Ba,
           crop_class = TREND_CATE,
           prop = perc) %>% # it's actually the proportion
    select(ID, basin, crop_class, prop) %>%
    mutate(crop_class = tolower(crop_class),
           crop_class = gsub(' ', '', crop_class)) %>% 
    filter(basin != 'Suisun') %>% 
    filter(!(crop_class == 'corn' & basin %in% c('San Joaquin', 'Tulare'))) %>% 
    group_by(crop_class) %>% 
    summarize(prop = sum(prop), .groups = 'drop')
  
  left_join(df, prop, by = 'crop_class') %>%
    mutate(ha = acres * prop / 2.47105) %>%
    select(crop_class, year, ha) %>%
    mutate(crop_class = recode(crop_class,
                               fieldcrop = 'other',
                               rowcrop = 'other',
                               grains = 'other')) %>% 
    group_by(crop_class, year) %>% 
    summarize(ha = sum(ha), .groups = 'drop')
    
}


add_wetland_totals <- function(df, seas = 67849.127, perm = 6986.170) {
  wetlands <- tibble(year = unique(df$year),
                     seas = seas,
                     perm = perm, 
                     wetlands = seas + perm) %>% 
    pivot_longer(seas:wetlands, names_to = 'habitat', values_to = 'ha')
  
  bind_rows(df %>% rename(habitat = crop_class),
            wetlands) %>% 
    arrange(habitat, year)
}
