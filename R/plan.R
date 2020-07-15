the_plan <-
  drake_plan(
    # INCENTIVE PROGRAMS-----------
    # Compile data on incentive program time series
    brdat = anonymize_BR_spatialdat(file_in('data/GIS/CONFIDENTIAL_br_fieldsMaster/CONFIDENTIAL_br_fieldsMaster_20190226.shp')) %>% 
      cleanup_BR_dat() %>% 
      get_BR_dates() %>% 
      filter(season != 'winter') %>% # drop Jan 2015 program (not managed for shorebird depth)
      get_BR_timeseries(drawdown = 14) %>%  #assume 2 weeks of gradual drawdown
      estimate_BR_compliance(rates = file_in('data/BR_compliance.csv')),
    
    whepdat = get_WHEP_data(file_in('data/WHEP_summary.xlsx'), cols = c(9:12)) %>% 
      filter(practice != 'WHEP_boardsin') %>% 
      # add missing data from fall 2013
      bind_rows(tibble(practice = 'WHEP_fall', 
                       fiscalyear = 2013, 
                       ha = 258/2.47105, 
                       bioyear = 2013)) %>% 
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
    
    # FLOOD CURVES-----------
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
    
    # summary stats:
    floodpred_peaks = floodpred_by_year %>% 
      filter(habitat != 'other' & yday < 300) %>% # to avoid choosing rice peak in late spring
      group_by(habitat, group) %>% 
      mutate(peak = yday[fit == max(fit)]) %>% 
      filter(yday == peak) %>% 
      select(habitat:ucl) %>% 
      write_csv('output/floodcurve_peaks.csv'),
    
    # HABITAT AVAILABILITY------------
    # estimate daily habitat changes in each year:
    change = estimate_habitat_change(
      basepath = file_in('data/cvjv_orig/baseline_potential_habitat.csv'),
      depthpath = file_in('data/cvjv_orig/depth_curves.csv'),
      floodcurves = floodpred_by_year,
      time = 'yday', value = 'fit', wetsplit = TRUE) %>% 
      append_incentives(brdat, whepdat),
    
    habitat_avail = compile_habitat_availability(change),
    
    # EFFORT TABLE: total incentives and total ha-days of accessible open water
    effort = bind_rows(map_dfr(change, ~.x[['added']], .id = 'group') %>% 
                         group_by(group) %>% 
                         summarize_at(vars(br_fall:whep_vardd), ~sum(.)) %>% 
                         mutate(incentives = br_fall + br_spring + 
                                  whep_fall + whep_vardd,
                                label = 'total'),
                       habitat_avail %>% filter(watertype == 'accessible') %>% 
                         group_by(group) %>% 
                         summarize_at(vars(incentives:whep_vardd), ~sum(.)) %>% 
                         mutate(label = 'accessible')) %>% 
      write_csv(file_out('output/incentive_effort.csv')),
    
    # FIT MODEL--------
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
    
    # extract consumption tables
    consumed = map_dfr(models,
                       ~map_dfr(.x, ~.x[['energy.consumed']], .id = 'group'),
                       .id = 'scenario') %>% 
      separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
      mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
             incentives = recode(incentives, free = 'without')),
    
    # MONTE CARLO-----------
    # resample flooding curves & prop.perm 10k times (slow!)
    #   result should be a named list with elements for each
    #   landcover type; each list contains estimates for each time step (rows) and
    #   simulation (columns)
    floodsim = resample_floodcurves(
      floodmod_by_year, seas = 67849.127, perm = 6986.170) %>% 
      add_original_resamples(
        origsimpath = file_in('data/cvjv_orig/resamples.RData')),
    
    depthsim = resample_depthcurves(
      origsimpath = file_in('data/cvjv_orig/resamples.RData'),
      brdat, whepdat),
    
    energysim = resample_energydens(
      energypath = file_in('data/cvjv_orig/energy_content.csv')),
    
    # returns an array, nsims deep
    models_mc = run_mc_all(
      needspath = file_in('data/cvjv_orig/daily_energy_requirement.csv'),
      basepath = file_in('data/cvjv_orig/baseline_potential_habitat.csv'),
      energysim = energysim, floodsim = floodsim, depthsim = depthsim, 
      nsim = 1000, days = 319, 
      groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
      brdat, whepdat),
    
    # compile shortfalls: find mean, lcl, ucl for each scenario and time step
    # across all iterations (compared to original point estimates)
    shortfallsum_mc = map2_dfr(models_mc, 
                               energysum %>% 
                                 mutate(scenario = factor(scenario, 
                                                          levels = names(models_mc))) %>% 
                                 split(.$scenario),
                               ~summarize_mc(mc = .x,
                                             element = 'energy', 
                                             var = 'shortfall',
                                             groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
                                             days = 319,
                                             by = 'year',
                                             origdat = .y),
                               .id = 'scenario') %>% 
      separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
      mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
             incentives = recode(incentives, free = 'without')),
    
    shortfall_byseason_mc = map2_dfr(models_mc, 
                                     energysum %>% 
                                       mutate(scenario = factor(scenario, 
                                                                levels = names(models_mc))) %>% 
                                       split(.$scenario),
                                     ~summarize_mc(mc = .x, 
                                                   element = 'energy', 
                                                   var = 'shortfall', 
                                                   groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
                                                   days = 319, by = 'season',
                                                   origdat = .y),
                                     .id = 'scenario') %>% 
      separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
      mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
             incentives = recode(incentives, free = 'without')),

    # compile estimated totals of accessible habitat (compared to original point estimates)
    accessible_mc = map_dfr(c('wetlands', 'rice', 'corn', 'other', 
                              'br_fall', 'br_spring', 'whep_fall', 
                              'whep_vardd', 'incentives', 'totalfree') %>% set_names(),
                            ~summarize_mc(models_mc$obj_with, 
                                          element = 'accessible', 
                                          var = .x,
                                          groups = c('2013-14', '2014-15', 
                                                     '2015-16', '2016-17'),
                                          days = 319, by = 'year',
                                          orighabitat = habitat_avail %>% 
                                            filter(watertype == 'accessible')),
                            .id = 'habitat'),

    # FILL SHORTFALLS-----------
    # if new habitat has same energy density as wetlands:
    fill_wetlands = fill_all_shortfalls(
      population = c('baseline', 'objectives'),
      group = c('2013-14', '2014-15', '2015-16', '2016-17'),
      newhabitatenergy = 'wetlands',
      needspath = file_in('data/cvjv_orig/daily_energy_requirement.csv'), 
      energypath = file_in('data/cvjv_orig/energy_content.csv'),
      orighabitatchange = change, 
      drawdown = 14,
      newhabitatpath = file_in('data/filling_shortfalls_framework.csv'), 
      limit = 100000) %>% 
      separate(label, into = c('group', 'population'), sep = '_'),
    
    # if new habitat has same energy density as rice:
    fill_rice = fill_all_shortfalls(
      population = c('baseline', 'objectives'),
      group = c('2013-14', '2014-15', '2015-16', '2016-17'),
      newhabitatenergy = 'rice',
      needspath = file_in('data/cvjv_orig/daily_energy_requirement.csv'), 
      energypath = file_in('data/cvjv_orig/energy_content.csv'),
      orighabitatchange = change, 
      drawdown = 14,
      newhabitatpath = file_in('data/filling_shortfalls_framework.csv'), 
      limit = 100000) %>% 
      separate(label, into = c('group', 'population'), sep = '_'),
    
    # double-check there are no shortfalls, and save energy supply results for
    # plotting
    test_wetlands = test_added_habitat(
      needspath = file_in('data/cvjv_orig/daily_energy_requirement.csv'),
      energypath = file_in('data/cvjv_orig/energy_content.csv'),
      filledhabitat = fill_wetlands  %>% 
        pivot_wider(names_from = population, values_from = filled),
      newhabitatenergy = 'wetlands',
      orighabitatchange = change),
    
    test_rice = test_added_habitat(
      needspath = file_in('data/cvjv_orig/daily_energy_requirement.csv'),
      energypath = file_in('data/cvjv_orig/energy_content.csv'),
      filledhabitat = fill_rice  %>% 
        pivot_wider(names_from = population, values_from = filled),
      newhabitatenergy = 'rice',
      orighabitatchange = change),

    # fill_wetlands_hab = fill_wetlands %>% 
    #   split(.$population) %>% 
    #   map(~update_habitat_change(original = change, filled = .x, 
    #                             column = "filled", drawdown = 0)) %>% 
    #   map_dfr(~map_dfr(.x, ~.x[['accessible']], .id = 'group'),
    #           .id = 'population'),
    
    fill_rice_hab = fill_rice %>% 
      split(.$population) %>% 
      map(~update_habitat_change(original = change, filled = .x, 
                                 column = "filled", drawdown = 0)) %>% 
      map_dfr(~map_dfr(.x, ~.x[['accessible']], .id = 'group'),
              .id = 'population'),
    
    # CREATE TABLES--------------
    
    table_effort = make_habitat_table(effort, 
                                      pathout = file_out('doc/table_habitat_accessible.docx')),
    
    table_habitat = make_habitat_table(accessible_mc,
                                       pathout = file_out('doc/table_habitat_accessible.docx')),
    
    table_shortfalls = make_shortfall_table(shortfallsum_mc, 
                                            shortfall_byseason_mc, 
                                            pathout = file_out('doc/table_shortfalls.docx')),
    
    # PLOT RESULTS---------------
    fig_landcover = plot_landcover(
      nassdat, scale = 1000, ymax = 800,
      palette = c('gray20', 'gray40', 'gray60', 'black'),
      ylab = 'Total area (ha, thousands)',
      filename = file_out('figs/baseline_habitat_ms.png'),
      width = 80, height = 60, dpi = 400),
    
    fig_floodcurve = plot_floodcurves(
      floodpred_by_year, 
      filename = file_out('figs/flood_curves_ms.png'),
      width = 80, height = 140, dpi = 400),
    
    fig_habitat = plot_habitat(
      habitat_avail, scale = 1000, ymax = 250,
      levels = c('incentives', 'rice', 'corn', 'other', 'wetlands'),
      ylabs = c('Open water (ha, thousands)',
                'Accessible open water (ha, thousands)'),
      filename = file_out('figs/habitat_ms.png'), 
      width = 169, height = 180, dpi = 400),
    
    fig_shortsum = plot_shortfalls(
      energysum, habitat_avail, 
      needspath = file_in('data/cvjv_orig/daily_energy_requirement.csv'),
      scale = 1000000, ymax = 280,
      fillpalette = c('without' = 'gray80', 'with' = 'gray50'),
      segmentpalette = c('black', 'gray60'),
      ylab = 'Energy shortfall (kJ, millions)',
      filename = file_out('figs/energy_shortfall_by_year_ms.png'),
      width = 169, height = 180, dpi = 400),
    
    fig_shorttimeline = plot_shortfall_timeline(
      energysum, size = 12, interval = 'week',
      filename = file_out('figs/energy_shortfall_timeline.png'),
      width = 169, height = 30, dpi = 400),
    
    # plot_incentive_effects(energysum, consumed, habitat_avail),
    
    fig_filled = plot_filled_habitat(
      fill_rice_hab, scale = 1000, 
      ylab = 'Additional habitat needed (ha, thousands)',
      filename = file_out('figs/filled_shortfalls.png'),
      width = 80, height = 100, dpi = 400)
)
