the_plan <-
  drake_plan(
    # Compile data on incentive program time series
    brdat = anonymize_BR_spatialdat(file_in('data/GIS/CONFIDENTIAL_br_fieldsMaster/CONFIDENTIAL_br_fieldsMaster_20190226.shp')) %>% 
      cleanup_BR_dat() %>% 
      get_BR_dates() %>% 
      filter(season != 'winter') %>% # drop Jan 2015 program (not managed for shorebird depth)
      get_BR_timeseries(drawdown = 14) %>%  #assume 2 weeks of gradual drawdown
      estimate_BR_compliance(rates = file_in('data/BR_compliance.csv')),
    
    whepdat = get_WHEP_data(file_in('data/WHEP_summary.xlsx'), cols = c(9:12)) %>% 
      filter(practice != 'WHEP_boardsin') %>% 
      get_WHEP_timeseries(drawdown = 14) %>% 
      estimate_WHEP_compliance(rates = file_in('data/cvjv_orig/depth_curves.csv')),
    
    # Acquire annual ag statistics from NASS Quickstats:
    #  annual totals of rice, corn, other field crops (beans, barley, oats,
    #  safflower, sorghum, sugarbeets, wheat), and row crops (vegetable totals),
    nassdat = map_df(c(2013:2016), extract_nass, cvjv.match = FALSE) %>% 
      summarize_cropclass() %>% 
      estimate_cvjv_croptotals(
        proportions = file_in('data/cvjv_orig/ag_distribution_basins.csv')) %>% 
      add_wetland_totals(seas = 67849.127, perm = 6986.170),
    
    # Get data from water tracker and subtract area of incentive programs from
    # flooded rice
    flooddat = get_waterdat(path = file_in('data/stats_basin_ag.csv'),
                            metapath = file_in('data/basin_stats_unfilled.csv')) %>% 
      subtract_incentives(brdat, whepdat),
    
    # Fit GAMMs
    # proportion flooded by day of year, excluding incentive acres
    # drop those with relatively few pixels estimated, weight by prop.cloudfree
    floodmod_by_year = flooddat %>% filter(prop.sampled >= 0.75) %>%
      filter(habitat != 'other') %>% # do not update the flood curve for "other crops"
      # filter(habitat == 'wetlands' | yday > 62) %>% # drop anything before 1 Sept unless wetlands
      split(.$habitat, drop = TRUE) %>% 
      map(~ fit_gamm4(df = ., nwater = 'nflooded_free', dayofyear = 'yday',
                      year = 'bioyear', weights = 'prop.cloudfree', 
                      by = 'group', plot = FALSE)),
    
    # extract predicted values:
    floodpred_by_year = floodmod_by_year %>%
      map_dfr(function(x) {x$pred}, .id = 'habitat') %>%
      mutate(habitat = factor(habitat, levels = c('wetlands', 'rice', 'corn'))) %>% 
      # estimate proportion of flooded wetlands that are semiperm vs. seasonals
      estimate_proportion_perm(seas = 67849.127, perm = 6986.170) %>% 
      # add estimates from original curve for "other" crop
      add_original_curves(
        origpath = file_in('data/cvjv_orig/flooding_curves.csv'),
        subset = 'other'),
    
    #estimate daily habitat changes in each year:
    change = estimate_habitat_change(
      basepath = file_in('data/cvjv_orig/baseline_potential_habitat.csv'),
      depthpath = file_in('data/cvjv_orig/depth_curves.csv'),
      floodcurves = floodpred_by_year,
      time = 'yday', value = 'fit', wetsplit = TRUE) %>% 
      append_incentives(brdat, whepdat),
    
    habitat_avail = compile_habitat_availability(change),
    
    # fit bioenergetics model
    models = fit_bioenergetics_model(
      needspath = file_in('data/cvjv_orig/daily_energy_requirement.csv'),
      energypath = file_in('data/cvjv_orig/energy_content.csv'),
      habitatchange = change),
    
    # extract shortfall tables
    energysum = map_dfr(models, 
                        ~map_dfr(.x, ~.x[['energy']], .id = 'group'), 
                        .id = 'scenario') %>% 
      separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
      mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
             incentives = recode(incentives, free = 'without')) %>% 
      write_csv(file_out('output/bioenergetics_results_energy.csv')),
    
    # PLOT RESULTS
    plot_landcover(nassdat, scale = 1000, ymax = 800,
                   palette = c('gray20', 'gray40', 'gray60', 'black'),
                   ylab = 'Total area (ha, thousands)',
                   filename = file_out('figs/baseline_habitat_ms.png'),
                   width = 80, height = 60, dpi = 400),
    
    plot_floodcurves(floodpred_by_year, 
                     filename = file_out('figs/flood_curves_ms.png'),
                     width = 80, height = 140, dpi = 400),
    
    plot_habitat(habitat_avail, scale = 1000, ymax = 250,
                 levels = c('incentives', 'rice', 'corn', 'other', 'wetlands'),
                 ylabs = c('Open water (ha, thousands)',
                           'Accessible open water (ha, thousands)'),
                 filename = file_out('figs/habitat_ms.png'), 
                 width = 169, height = 180, dpi = 400),
    
    plot_shortfalls(energysum, habitat_avail, scale = 1000000, ymax = 250,
                    fillpalette = c('without' = 'gray80', 'with' = 'gray50'),
                    segmentpalette = c('black', 'gray60'),
                    ylab = 'Energy shortfall (kJ, millions)',
                    filename = file_out('figs/energy_shortfall_by_year_ms.png'),
                    width = 169, height = 180, dpi = 400)
)
