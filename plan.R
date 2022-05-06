purrr::map(paste0('R/', list.files('R')), source) #load packages and functions

# INCENTIVE PROGRAMS-----------
# Compile data on incentive program time series
brdat = anonymize_BR_spatialdat('data/GIS/CONFIDENTIAL_br_fieldsMaster/CONFIDENTIAL_br_fieldsMaster_20190226.shp') %>% 
  cleanup_BR_dat() %>% 
  get_BR_dates() %>% 
  filter(season != 'winter') %>% # drop Jan 2015 program (not managed for shorebird depth)
  get_BR_timeseries(drawdown = 14) %>%  #assume 2 weeks of gradual drawdown
  estimate_BR_compliance(rates = 'data/BR_compliance.csv') %>% 
  write_csv('data/brdat.csv')
   
whepdat = get_WHEP_data('data/WHEP_summary.xlsx', cols = c(9:12)) %>% 
  filter(practice != 'WHEP_boardsin') %>% 
  # add missing data from fall 2013
  bind_rows(tibble(practice = 'WHEP_fall', 
                   fiscalyear = 2013, 
                   ha = 258/2.47105, 
                   bioyear = 2013)) %>% 
  get_WHEP_timeseries(drawdown = 14) %>% 
  estimate_WHEP_compliance(rates = 'data/cvjv_orig/depth_curves.csv') %>% 
  write_csv('data/whepdat.csv')
    
# Acquire annual ag statistics from NASS Quickstats:
#  annual totals of rice, corn, other field crops (beans, barley, oats,
#  safflower, sorghum, sugarbeets, wheat), and row crops (vegetable totals),
nassdat = map_df(c(2013:2016), extract_nass, cvjv.match = FALSE) %>% 
  summarize_cropclass() %>% 
  estimate_cvjv_croptotals(
    proportions = 'data/cvjv_orig/ag_distribution_basins.csv') %>% 
  add_wetland_totals(seas = 67849.127, perm = 6986.170) %>% 
  write_csv('data/nassdat.csv')
    
# FLOOD CURVES-----------
# Get data from water tracker and subtract area of incentive programs from
# flooded rice
flooddat = get_waterdat(path = 'data/stats_basin_ag.csv',
                        metapath = 'data/basin_stats_unfilled.csv') %>% 
  subtract_incentives(brdat, whepdat)
    
# Fit GAMMs
# proportion flooded by day of year, excluding incentive acres
# drop those with relatively few pixels estimated, weight by prop.cloudfree
floodmod_by_year = flooddat %>% filter(prop.sampled >= 0.75) %>%
  filter(habitat != 'other') %>% # do not update the flood curve for "other crops"
  # filter(habitat == 'wetlands' | yday > 62) %>% # drop anything before 1 Sept unless wetlands
  split(.$habitat, drop = TRUE) %>% 
  map(~ fit_gamm4(df = ., nwater = 'nflooded_free', dayofyear = 'yday',
                  year = 'bioyear', weights = 'prop.cloudfree', 
                  by = 'group', plot = FALSE))
    
# extract predicted values:
floodpred_by_year = floodmod_by_year %>%
  map_dfr(function(x) {x$pred}, .id = 'habitat') %>%
  mutate(habitat = factor(habitat, levels = c('wetlands', 'rice', 'corn'))) %>% 
  # estimate proportion of flooded wetlands that are semiperm vs. seasonals
  estimate_proportion_perm(seas = 67849.127, perm = 6986.170) %>% 
  # add estimates from original curve for "other" crop
  add_original_curves(
    origpath = 'data/cvjv_orig/flooding_curves.csv',
    subset = 'other') %>% 
  write_csv('output/floodcurves.csv')
    
# summary stats:
floodpred_peaks = floodpred_by_year %>% 
  filter(habitat != 'other' & yday < 300) %>% # to avoid choosing rice peak in late spring
  group_by(habitat, group) %>% 
  mutate(peak = yday[fit == max(fit)]) %>% 
  filter(yday == peak) %>% 
  select(habitat:ucl) %>% 
  write_csv('output/floodcurve_peaks.csv')
    
# HABITAT AVAILABILITY------------
# estimate daily habitat changes in each year:
change = estimate_habitat_change(
  basepath = 'data/cvjv_orig/baseline_potential_habitat.csv',
  depthpath = 'data/cvjv_orig/depth_curves.csv',
  floodcurves = floodpred_by_year,
  time = 'yday', value = 'fit', wetsplit = TRUE) %>% 
  append_incentives(brdat, whepdat)
save(change, file = 'output/change.RData')
# change is a list of lists: one element for each year ("group"), within which
# are the elements: openwater, added, returned, accessible, prop.accessible;
# each of these is a df with columns for time (day of year) and each cover type

# compile openwater and accessible habitat for plotting; calculate total
# availability/accessible across all incentive programs
habitat_avail = compile_habitat_availability(change) %>% 
  write_csv('output/habitat_avail.csv')
    
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
  write_csv('output/incentive_effort.csv')
    
# FIT MODEL--------
models = fit_bioenergetics_model(
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv',
  energypath = 'data/cvjv_orig/energy_content.csv',
  habitatchange = change)
    
# extract shortfall tables
energysum = map_dfr(models, 
                    ~map_dfr(.x, ~.x[['energy']], .id = 'group'), 
                    .id = 'scenario') %>% 
  separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
  mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
         incentives = recode(incentives, free = 'without')) %>% 
  write_csv('output/bioenergetics_results_energy.csv')
    
# extract consumption tables
consumed = map_dfr(models,
                   ~map_dfr(.x, ~.x[['energy.consumed']], .id = 'group'),
                   .id = 'scenario') %>% 
  separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
  mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
         incentives = recode(incentives, free = 'without'))
    
# MONTE CARLO-----------
# resample flooding curves & prop.perm 10k times (slow!)
#   result should be a named list with elements for each
#   landcover type; each list contains estimates for each time step (rows) and
#   simulation (columns)

# # reconstitute original CVJV resamples from separate csv files stored on GitHub
# floodsim = list(
#   wetlands = read_csv('data/cvjv_orig/resamples/floodsim_wetlands.csv') %>% as.matrix(),
#   rice = read_csv('data/cvjv_orig/resamples/floodsim_rice.csv') %>% as.matrix(),
#   corn = read_csv('data/cvjv_orig/resamples/floodsim_corn.csv') %>% as.matrix(),
#   other = read_csv('data/cvjv_orig/resamples/floodsim_other.csv') %>% as.matrix(),
#   corn_north = read_csv('data/cvjv_orig/resamples/floodsim_corn_north.csv') %>% as.matrix()
# )
# depthsim = list(
#   rice = read_csv('data/cvjv_orig/resamples/depthsim_rice.csv') %>% as.matrix(),
#   corn = read_csv('data/cvjv_orig/resamples/depthsim_other.csv') %>% pull(corn),
#   other = read_csv('data/cvjv_orig/resamples/depthsim_other.csv') %>% pull(other),
#   perm = read_csv('data/cvjv_orig/resamples/depthsim_perm.csv') %>% as.matrix(),
#   seas = read_csv('data/cvjv_orig/resamples/depthsim_seas.csv') %>% as.matrix(),
#   corn_north = read_csv('data/cvjv_orig/resamples/depthsim_other.csv') %>% pull(corn_north)
# )
# energysim = read_csv('data/cvjv_orig/resamples/energysim.csv')
# save(floodsim, depthsim, energysim, file = 'data/resamples.RData')

floodsim = resample_floodcurves(
  floodmod_by_year, seas = 67849.127, perm = 6986.170) %>% 
  add_original_resamples( #for "other" field and row crops
    origsimpath = 'data/cvjv_orig/resamples.RData')


depthsim = resample_depthcurves(
  origsimpath = 'data/cvjv_orig/resamples.RData',
  brdat, whepdat)

energysim = resample_energydens(
  energypath = 'data/cvjv_orig/energy_content.csv')

save(floodsim, depthsim, energysim, file = 'output/resamples.RData')

# ARCHIVE VERSION (GIT-LARGE FILE STORAGE)
write_csv(as.data.frame(floodsim$wetlands), 'output/resamples/floodsim_wetlands.csv')
write_csv(as.data.frame(floodsim$rice), 'output/resamples/floodsim_rice.csv')
write_csv(as.data.frame(floodsim$corn), 'output/resamples/floodsim_corn.csv')
write_csv(as.data.frame(floodsim$other), 'output/resamples/floodsim_other.csv')
write_csv(as.data.frame(floodsim$prop.perm), 'output/resamples/floodsim_prop.perm.csv')
write_csv(as.data.frame(depthsim$rice), 'output/resamples/depthsim_rice.csv')
write_csv(as.data.frame(depthsim$perm), 'output/resamples/depthsim_perm.csv')
write_csv(as.data.frame(depthsim$seas), 'output/resamples/depthsim_seas.csv')
write_csv(as.data.frame(depthsim$whep_vardd), 'output/resamples/depthsim_whep_vardd.csv')
write_csv(as.data.frame(depthsim$whep_fall), 'output/resamples/depthsim_whep_fall.csv')
write_csv(as.data.frame(depthsim$br_fall), 'output/resamples/depthsim_br_fall.csv')
write_csv(as.data.frame(depthsim$br_spring), 'output/resamples/depthsim_br_spring.csv')
write_csv(data.frame(corn = depthsim$corn, other = depthsim$other),
          'output/resamples/depthsim_other.csv')
write_csv(as.data.frame(energysim), 'output/resamples/energysim.csv')


# returns an array, nsims deep
models_mc = run_mc_all(
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv',
  basepath = 'data/cvjv_orig/baseline_potential_habitat.csv',
  energysim = energysim, floodsim = floodsim, depthsim = depthsim, 
  nsim = 1000, days = 319, 
  groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
  brdat, whepdat)
  
save(models_mc, file = 'output/model_results_mc.RData')

# compile shortfalls: find mean, lcl, ucl for each scenario and time step
# across all iterations (compared to original point estimates)
# - by year:
shortfallsum_mc = map2_dfr(
  models_mc, 
  energysum %>% 
    mutate(scenario = factor(scenario, levels = names(models_mc))) %>% 
    split(.$scenario),
  ~summarize_mc(mc = .x, element = 'energy', var = 'shortfall', by = 'year',
                groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
                days = 319, origdat = .y),
  .id = 'scenario') %>% 
  separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
  mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
         incentives = recode(incentives, free = 'without')) %>% 
  write_csv('output/shortfallsum_mc.csv')

# - by season:
shortfall_byseason_mc = map2_dfr(
  models_mc, 
  energysum %>% 
    mutate(scenario = factor(scenario, levels = names(models_mc))) %>% 
    split(.$scenario),
  ~summarize_mc(mc = .x, element = 'energy', var = 'shortfall', by = 'season',
                groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
                days = 319, origdat = .y),
  .id = 'scenario') %>% 
  separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
  mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
         incentives = recode(incentives, free = 'without')) %>% 
  write_csv('output/shortfall_byseason_mc.csv')

# - by day:
shortfall_byday_mc = map2_dfr(
  models_mc,
  energysum %>% 
    mutate(scenario = factor(scenario, levels = names(models_mc))) %>% 
    split(.$scenario),
  ~summarize_mc(mc = .x, element = 'energy', var = 'shortfall', by = 'day',
                groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
                days = 319, origdat = .y),
  .id = 'scenario') %>% 
  separate(scenario, into = c('population', 'incentives'), remove = FALSE) %>% 
  mutate(population = recode(population, obs = 'baseline', obj = 'objectives'),
         incentives = recode(incentives, free = 'without')) %>% 
  write_csv('output/shortfall_byday_mc.csv')
    
# compile estimated totals of accessible habitat (compared to original point
# estimates)
accessible_mc = map_dfr(
  c('wetlands', 'rice', 'corn', 'other', 'br_fall', 'br_spring', 'whep_fall', 
    'whep_vardd', 'incentives', 'totalfree', 'grandtotal') %>% set_names(),
  ~summarize_mc(models_mc$obj_with, element = 'accessible', var = .x, 
                by = 'year',
                groups = c('2013-14', '2014-15', '2015-16', '2016-17'),
                days = 319, 
                origdat = habitat_avail %>% filter(watertype == 'accessible')),
  .id = 'habitat') %>% 
  write_csv('output/accessible_mc.csv')

# FILL SHORTFALLS-----------
# if new habitat has same energy density as wetlands:
fill_wetlands = fill_all_shortfalls(
  population = c('baseline', 'objectives'),
  group = c('2013-14', '2014-15', '2015-16', '2016-17'),
  newhabitatenergy = 'wetlands',
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv', 
  energypath = 'data/cvjv_orig/energy_content.csv',
  orighabitatchange = change, 
  drawdown = 14,
  newhabitatpath = 'data/filling_shortfalls_framework.csv', 
  limit = 100000) %>% 
  separate(label, into = c('group', 'population'), sep = '_')
    
# if new habitat has same energy density as rice:
fill_rice = fill_all_shortfalls(
  population = c('baseline', 'objectives'),
  group = c('2013-14', '2014-15', '2015-16', '2016-17'),
  newhabitatenergy = 'rice',
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv', 
  energypath = 'data/cvjv_orig/energy_content.csv',
  orighabitatchange = change, 
  drawdown = 14,
  newhabitatpath = 'data/filling_shortfalls_framework.csv', 
  limit = 100000) %>% 
  separate(label, into = c('group', 'population'), sep = '_') %>% 
  write_csv('output/fill_rice.csv')
    
# double-check there are no shortfalls, and save energy supply results for
# plotting
test_wetlands = test_added_habitat(
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv',
  energypath = 'data/cvjv_orig/energy_content.csv',
  filledhabitat = fill_wetlands  %>% 
    pivot_wider(names_from = population, values_from = filled),
  newhabitatenergy = 'wetlands',
  orighabitatchange = change)
    
test_rice = test_added_habitat(
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv',
  energypath = 'data/cvjv_orig/energy_content.csv',
  filledhabitat = fill_rice  %>% 
    pivot_wider(names_from = population, values_from = filled),
  newhabitatenergy = 'rice',
  orighabitatchange = change)

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
          .id = 'population')
write_csv(fill_rice_hab, 'output/fill_rice_hab.csv')

# CREATE TABLES--------------

table_effort = make_effort_table(
  effort, scale = 1000000,
  pathout = 'output/table_incentive_effort.docx')

table_habitat = make_habitat_table(
  accessible_mc, scale = 1000000,
  pathout = 'output/table_habitat_accessible.docx')

table_shortfalls = make_shortfall_table(
  shortfallsum_mc, shortfall_byseason_mc, scale = 1000000000,
  pathout = 'output/table_shortfalls.docx')

table_habitatneed = make_habitatneed_table(
  fill_rice, 
  newhabitatpath = 'data/filling_shortfalls_framework.csv',
  pathout = 'output/table_habitat_needs.docx')
    
# PLOT RESULTS---------------
# Specs for Ecological Monographs:

# Provide each figure a single time as a high-resolution image suitable for
# publication. Preferred file formats include TIF, EPS, PDF, or AI at 600 ppi,
# while JPEG, PPT/PPTX, or DOC/DOCX are acceptable if the resolution is
# sufficient. See Author Guidelines online for more details. Ultimately, we are
# looking for files that remain crisp and clear when the on-screen view is
# significantly zoomed in, and produce crisp, clean prints for readers that
# print the PDF version of the final publication.

# Please provide figures sized for PDF publication. Column width figures (3
# inches) are preferred when possible. For wide or multi-panel figures, provide
# images sized no larger than 6 inches wide x 8 inches high for portrait layout
# in the typeset PDF of your paper. All text should be no smaller than 6 point
# and no larger than 10 point when the image is sized for publication. For
# readability, we suggest using a text size hierarchy, sizing axis numbers
# between 6 and 7-point, axis labels between 8 and 9-point, panel labels that
# consist of words between 7 and 8 point, and panel labels that consist of a
# single letter at 10 point.

# fig_landcover = plot_landcover(
#   nassdat, scale = 1000, ymax = 800,
#   palette = c('gray20', 'gray40', 'gray60', 'black'),
#   ylab = 'Total area (ha, thousands)',
#   filename = 'figs/baseline_habitat_ms.png',
#   width = 80, height = 60, dpi = 400)

# Fig 1: Flood curves (column width - 3 inches)
plot_floodcurves(
  floodpred_by_year, 
  # read_csv('output/floodcurves.csv'),
  filename = 'figs/Fig1_floodcurves.tiff',
  width = 3, height = 5.25, units = 'in', dpi = 600)

# Fig 2: Open water and accessible water (double - 6 x 8 inches)
plot_habitat(
  # habitat_avail, 
  read_csv('output/habitat_avail.csv'),
  scale = 1000, ymax = 250,
  levels = c('incentives', 'rice', 'corn', 'other', 'wetlands'),
  ylabs = c('Open water (ha, thousands)',
            'Accessible open water (ha, thousands)'),
  col = c('brown', pointblue.palette[3], 'gold', pointblue.palette[c(2, 4)]),
  filename = 'figs/Fig2_habitat.tiff', 
  width = 6, height = 8, units = 'in', dpi = 600)

# Fig 3: Shortfalls with CI (double - 6 x 8 inches)
plot_shortfalls_ci(
  # energydf = shortfall_byday_mc,
  energydf = read_csv('output/shortfall_byday_mc.csv'),
  # habitatdf = habitat_avail,
  habitatdf = read_csv('output/habitat_avail.csv'),
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv',
  scale = 1000000, ymax = 280,
  ylab = 'Energy shortfall (kJ, millions)',
  filename = 'figs/Fig3_shortfalls.tiff',
  width = 6, height = 8, units = 'in', dpi = 600)

# Fig 4: Effects of incentives on shortfalls (double - 6 x 8 inches)
plot_shortfalls(
  # energydf = energysum,
  energydf = read_csv('output/bioenergetics_results_energy.csv'),
  # habitatdf = habitat_avail,
  habitatdf = read_csv('output/habitat_avail.csv'),
  needspath = 'data/cvjv_orig/daily_energy_requirement.csv',
  scale = 1000000, ymax = 280,
  ylab = 'Energy shortfall (kJ, millions)',
  filename = 'figs/Fig4_shortfall_comparison.tiff',
  width = 6, height = 8, units = 'in', dpi = 600)

# Fig 5: Color timeline (double wide but short - 6 x 2?)
plot_shortfall_timeline(
  # shortfall_byday_mc %>% ungroup(), 
  read_csv('output/shortfall_byday_mc.csv'),
  size = 10, interval = 'week_ci',
  col = c('always' = 'brown',
          'consistent' = pointblue.palette[3],
          'sometimes' = 'gold',
          'uncertain' = 'chartreuse3',
          'none' = 'chartreuse3'),
  filename = 'figs/Fig5_shortfall_timeline.tiff',
  width = 6, height = 1, units = 'in', dpi = 600)

# Fig 6: Color filled habitat needed (3 inches wide)
plot_filled_habitat(
  # fill_rice_hab, 
  read_csv('output/fill_rice_hab.csv'),
  scale = 1000, 
  ylab = 'Additional habitat needed (ha, thousands)',
  col = c('gold', 'brown'),
  filename = 'figs/Fig6_habitatneed.tiff',
  width = 3, height = 3.75, units = 'in', dpi = 600)



