extract_nass = function(t) {
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
              token = Sys.getenv('NASS_TOKEN' )),
    ## plus beans
    nass_data(source_desc = 'SURVEY', 
              sector_desc = 'CROPS', 
              commodity_desc =  'BEANS', 
              class_desc = 'DRY EDIBLE',
              prodn_practice_desc = 'ALL PRODUCTION PRACTICES',
              statisticcat_desc = 'AREA PLANTED',
              agg_level_desc = 'STATE',
              state_name = 'CALIFORNIA', 
              year = as.character(t), 
              reference_period_desc = 'YEAR',
              token = Sys.getenv('NASS_TOKEN' )),
    ## and alfalfa
    nass_data(source_desc = 'SURVEY', 
              sector_desc = 'CROPS', 
              commodity_desc =  'HAY & HAYLAGE', 
              statisticcat_desc = 'AREA PLANTED',
              agg_level_desc = 'STATE',
              state_name = 'CALIFORNIA', 
              year = as.character(t), 
              reference_period_desc = 'YEAR',
              token = Sys.getenv('NASS_TOKEN' )),
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
                token = Sys.getenv('NASS_TOKEN' ))
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
                token = Sys.getenv('NASS_TOKEN' )) 
    }
  )
}