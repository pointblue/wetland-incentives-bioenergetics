##' @title Use Monte Carlo analysis to estimate uncertainty in bioenergetics
##'   model results
##' @param floodmodels list containing flooding curve model results by land
##'   cover type
##' @param seas total estimated area of seasonal wetlands (hectares); see
##'   estimate_proportion_perm
##' @param perm total estimated area of permanent/semi-permanent wetlands
##'   (hectares); see estimate_proportion_perm
##' @param floodsim results of resample_floodcurves
##' @param depthsim results of resample_depthcurves
##' @param energysim results of resample_energydens
##' @param origsimpath results of prior resampling
##' @param energypath file path to CSV containing energy density estimates for
##'   each land cover, including "lcl" and "ucl" fields
##' @param needspath file path to CSV containing daily energy requirements;
##'   expects fields "yday", "DER.obj" (for population objectives), and
##'   "DER.obs" (for baseline "observed" population)
##' @param basepath file path to CSV containing estimates of total area (ha) of
##'   each land cover type
##' @param nsim number of Monte Carlo simulations to run (up to the max of
##'   10,000 provided in floodsim, depthsim, energysim, and origsimpath)
##' @param days number of time steps included in the model
##' @param groups character vector containing the names of the groups within
##'   which floodsim is calculated
##' @param population either "baseline" or "population", indicating which vector
##'   of energy needs are used in the model
##' @param ... tibble objects containing time series data on acres enrolled in
##'   incentive programs (to be appended to background baseline estimates of
##'   habitat availability); see estimate_habitat_change
##' @description Functions to re-run bioenergetics models with resampled
##'   parameters to estimate uncertainty in flood curves and depth curves (for
##'   non-incentive-flooded acres), and energy content per flooded acre (for
##'   all); assuming perfect information for change in habitat available from
##'   incentive acres

loading <- function(dataset){
  dataname <- load(dataset)
  return(mget(dataname))
}

resample_floodcurves <- function(floodmodels, seas = 67849.127, perm = 6986.170) {
  floodsim <- floodmodels %>%
    map(function(x) {
      sim <- plogis(x$Xp %*% t(x$br))
    })
  
  ## split wetlands in the resamples too:
  floodsim$prop.perm <- floodsim$wetlands %>% as_tibble() %>%
    mutate(habitat = 'wetlands',
           yday = floodmodels$wetlands$pred$yday,
           group = floodmodels$wetlands$pred$group) %>%
    pivot_longer(starts_with("V"), names_to = 'iteration', values_to = 'fit') %>%
    group_by(iteration) %>% 
    # function from update_flooding_curves.R:
    group_modify(~estimate_proportion_perm(df = .x, seas = seas, perm = perm)) %>%
    select(-fit) %>% 
    mutate(iteration = as.numeric(gsub('V', '', iteration))) %>% 
    arrange(iteration) %>% 
    pivot_wider(names_from = 'iteration', values_from = 'prop.perm') %>%
    arrange(group, yday) %>%
    select(-yday, -group, -habitat) %>%
    as.matrix()
  
  # fix dimnames
  dimnames(floodsim$prop.perm) <- dimnames(floodsim$wetlands)
  
  return(floodsim)
}

# # visualize as an example:
# apply(floodsim$rice, 1, quantile, c(0.025, 0.5, 0.975)) %>% t() %>% 
#   as.data.frame() %>% 
#   bind_cols(floodmodels$rice$pred %>% select(yday, group)) %>% 
#   rename(lcl = '2.5%', median = '50%', ucl = '97.5%') %>% 
#   ggplot(aes(yday, median, ymin = lcl, ymax = ucl, fill = group, color = group)) + 
#   geom_ribbon(alpha = 0.5) + geom_line()

add_original_resamples <- function(floodsim, origsimpath) {
  # add resamples for "other" field and row crops
  origsim <- loading(origsimpath)
  # how many years of data in floodsim?
  reps = dim(floodsim[[1]])[1]/nrow(origsim$floodsim$other)
  # add:
  floodsim$other = origsim$floodsim$other[rep(seq_len(nrow(origsim$floodsim$other)), reps),]
  return(floodsim)
}

resample_depthcurves <- function(origsimpath, ...) {
  # original depth curve resamples (for wetlands, rice, corn)
  origsim <- loading(origsimpath)
  depthsim <- origsim$depthsim
  depthsim$corn <- depthsim$corn_north
  depthsim$corn_north <- NULL
  
  # add depth curve resamples for incentives
  depthsim$whep_vardd <- depthsim$rice #treat as equivalent
  
  # resample BR and WHEP fall depth curves (same approach as for wetlands in
  # original CVJV)
  tmp_incentives <- bind_rows(...) %>% 
    filter(habitat != 'whep_vardd') %>% 
    select(habitat, group, yday, prop.accessible) %>% 
    filter(group == '2013-14') %>% 
    select(-group) %>% 
    complete(habitat, yday, fill = list(prop.accessible = NA)) %>% 
    # try adjusting whep_fall down so this process can generate some error
    mutate(prop.accessible = case_when(habitat == 'whep_fall' ~ 0.95,
                                       TRUE ~ prop.accessible)) %>% 
    split(.$habitat) %>% 
    map(function(x) {
      zp = rlogis(10000, location=0, scale=0.25)
      zp = sapply(zp, function(y) {plogis(qlogis(x$prop.accessible) + y)})
    })
  depthsim$br_fall = tmp_incentives$br_fall
  depthsim$br_spring = tmp_incentives$br_spring
  depthsim$whep_fall = tmp_incentives$whep_fall
  
  return(depthsim)
}

resample_energydens <- function(energypath) {
  energydens <- read_csv(energypath, col_types = cols()) %>%
    filter(habitat != 'corn') %>%
    mutate(habitat = recode(habitat, corn_north = 'corn'),
           habitat = factor(habitat, levels = c('perm', 'seas', 'rice', 'corn', 
                                                'other', 'whep_vardd', 'whep_fall', 
                                                'br_spring', 'br_fall')))  
  # -- add missing values for br and whep equivalent to rice:
  energydens <- energydens %>% 
    complete(habitat, 
             fill = list(value = energydens$value[energydens$habitat == 'rice'], 
                         lcl = energydens$lcl[energydens$habitat == 'rice'], 
                         ucl = energydens$ucl[energydens$habitat == 'rice']))
  # -- resample from log normal distribution based on standard deviation
  energydens %>% 
    mutate(logvalue = log(value/1000),
           logsd = (log(value/1000) - log(lcl/1000)) / 2) %>% 
    split(.$habitat) %>%
    map(function(x) {exp(rnorm(10000, mean = x$logvalue, sd = x$logsd)) * 1000})
}

run_mc_all <- function(needspath, basepath, energysim, floodsim, depthsim,
                       nsim, days = 319, 
                       groups = c('2013-14', '2014-15', '2015-16', '2016-17'), 
                       ...) {
  
  ## read in energy needs & base land cover acreages
  needs <- read_csv(needspath, col_types = cols())
  
  base <- read_csv(basepath, col_types = cols()) %>% 
    filter(!habitat %in% c('corn', 'seas', 'perm')) %>% 
    mutate(habitat = recode(habitat, corn_north = 'corn')) %>% 
    list()
  
  nsim <- min(nsim, dim(floodsim[[1]])[2])
  
  ## with population objectives:
  cat('\n\nScenario 1: with incentives for population objectives\n')
  res1 <- run_mc(needs = needs$DER.obj, base = base, energysim = energysim, 
                 floodsim = floodsim, depthsim = depthsim, nsim = nsim, days = days,
                 groups = groups, ...)
  
  ## with baseline:
  cat('\n\nScenario 2: with incentives for baseline population\n')
  res2 <- run_mc(needs = needs$DER.obs, base = base, energysim = energysim, 
                 floodsim = floodsim, depthsim = depthsim, nsim = nsim, days = days,
                 groups = groups, ...)
  
  ## repeat without incentive acres:
  ## - with population objectives:
  cat('\n\nScenario 3: without incentives for population objectives\n')
  res3 <- run_mc(needs = needs$DER.obj, base = base, energysim = energysim, 
                 floodsim = floodsim, depthsim = depthsim, nsim = nsim, days = days,
                 groups = groups)
  
  ## with baseline:
  cat('\n\nScenario 4: without incentives for baseline population\n')
  res4 <- run_mc(needs = needs$DER.obs, base = base, energysim = energysim, 
                 floodsim = floodsim, depthsim = depthsim, nsim = nsim, days = days,
                 groups = groups)
  
  res <- list(res1, res2, res3, res4)
  names(res) = c('obj_with', 'obs_with', 'obj_free', 'obs_free')
  return(res)
}

run_mc <- function(needs, base, energysim, floodsim, depthsim, nsim, 
                   days, groups, ...) {

  pb <- txtProgressBar(min = 0, max = nsim, initial = 0, style = 3)
  # pb <- progress_bar$new(total = nsim, clear = FALSE, show_after = 0,
  #                        format = "[:bar] :percent in :elapsedfull")
  # pb$tick(0)
  results <- map(c(1:nsim), bioenergmod_mc_custom, 
                 base = base, floodsim = floodsim, depthsim = depthsim,
                 energysim = energysim, days = days, groups = groups,
                 energyneed = needs, pb = pb, ...)
  close(pb)

  # compile results across all simulations into common arrays
  final = list()
  final$energy = map(results, function(x) {x[["energy"]]}) %>%
    abind::abind(along = 3)
  final$energy.supply = map(results, function(x) {x[["energy.supply"]]}) %>%
    abind::abind(along = 3)
  # final$energy.accessible = map(results, function(x) {x[["energy.accessible"]]}) %>%
  #   abind::abind(along = 3)
  # final$energy.consumed = map(results, function(x) {x[["energy.consumed"]]}) %>%
  #   abind::abind(along = 3)
  # final$energy.lost = map(results, function(x) {x[["energy.lost"]]}) %>%
  #   abind::abind(along = 3)
  final$openwater = map(results, function(x) {x[["openwater"]]}) %>% 
    abind::abind(along = 3)
  final$accessible = map(results, function(x) {x[["accessible"]]}) %>% 
    abind::abind(along = 3)
  return(final)
}

bioenergmod_mc_custom <- function(i, base, floodsim, depthsim, energysim,
                                  days, groups, energyneed, pb, ...) {

  changesim <- simulate_habitat_change(
    i, base = base, floodsim = floodsim, depthsim = depthsim, days = days, 
    groups = groups)

  args <- list(...)
  if (!is_empty(args)) {
    # append data from incentive programs
    add <- bind_rows(...) %>% 
      select(habitat, group, yday, available:returned) 
    
    # replace prop.accessible with resampled value
    add_mc <- add %>% 
      left_join(depthsim[which(names(depthsim) %in% add$habitat)] %>% 
                  map_dfr(function(x) {
                    if (is.null(dim(x))) {
                      data.frame(yday = c(1:days),
                                 prop.accessible = x[i])
                    } else {
                      data.frame(yday = c(1:days),
                                 prop.accessible = x[, i])
                    }
                  }, .id = 'habitat'),
                by = c('habitat', 'yday')) %>% 
      mutate(prop.accessible = replace_na(prop.accessible, 1),
             accessible = available * prop.accessible)
    
    changesim <- changesim %>% append_incentives(add_mc)
 
    changesim_open <- map_dfr(changesim, ~.x$openwater) %>% 
      mutate(incentives = br_fall + br_spring + whep_fall + whep_vardd)
    changesim_acc <- map_dfr(changesim, ~.x$accessible) %>% 
      mutate(incentives = br_fall + br_spring + whep_fall + whep_vardd)
  } else {
    changesim_open <- map_dfr(changesim, ~.x$openwater)
    changesim_acc <- map_dfr(changesim, ~.x$accessible)
  }
  
  energydens <- map_dfr(energysim, function(x) {
    data.frame(value = x[i])
  }, .id = 'habitat')
  
  # run model for each group (year) in changesim
  model <- map_df(changesim, 
                  ~ bioenergmod::run_bioenergmod_loop(
                    habitat.available = .x$openwater,
                    habitat.accessible = .x$accessible,
                    habitat.added = .x$added,
                    habitat.returned = .x$returned,
                    prop.accessible = .x$prop.accessible,
                    energyneed = energyneed,
                    energydens = energydens), .id = 'group')
  
  # include simulated openwater and accessible habitat in results
  model[['openwater']] = changesim_open
  model[['accessible']] = changesim_acc
  
  # update progress bar:
  # pb$tick()
  setTxtProgressBar(pb, i)
  return(model)
}

simulate_habitat_change = function(i, base, floodsim, depthsim, days, groups) {

  # extract proportion open water and prop.perm for simulation i (for all years
  # and land cover types)
  flood <- map_dfr(floodsim, function(x) {
    data.frame(yday = rep(c(1:days), length(groups)), 
               group = rep(groups, each = days),
               fit = x[, i])
  }, .id = 'habitat')
  
  pp <- flood %>% filter(habitat == 'prop.perm') %>%
    mutate(habitat = recode(habitat, prop.perm = 'wetlands')) %>%
    rename(prop.perm = fit)
  
  flood <- flood %>% filter(habitat != 'prop.perm') %>%
    full_join(pp, by = c('habitat', 'yday', 'group')) %>% 
    split(.$group)
  
  # repeat for proportion suitable depth: daily curves for all but corn and
  # "other" crops
  depth <- map_dfr(depthsim, function(x) {
    if (is.null(dim(x))) {
      data.frame(time = c(1:days),
                 prop.accessible = x[i])
    } else {
      data.frame(time = c(1:days),
                 prop.accessible = x[, i])
    }
  }, .id = 'habitat')
  
  # for each year in simulated floodcurves, calculate habitat change
  changesim <- map2(base, flood, bioenergmod::calculate_habitat_change,
                    time = 'yday', value = 'fit',
                    accessible = depth, wetsplit = TRUE)
  names(changesim) = names(flood)
  
  ## assume rice, corn, and other crops are not accessible prior to Sept 1
  ## (day 63)
  changesim$`2013-14`$added[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2014-15`$added[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2015-16`$added[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2016-17`$added[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2013-14`$returned[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2014-15`$returned[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2015-16`$returned[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2016-17`$returned[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2013-14`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2014-15`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2015-16`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2016-17`$openwater[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2013-14`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2014-15`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2015-16`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  changesim$`2016-17`$accessible[1:62, c('rice', 'corn', 'other')] = 0
  return(changesim)
}

summarize_mc <- function(mc, element, var, groups, days, by = 'day', origdat = NULL) {
  if (by == 'day') {
    if (var %in% dimnames(mc[[element]])[[2]]) {
      origdat %>% select(group, time, total = !!var) %>% 
        group_by(group, time) %>% 
        summarize_at(vars(-group_cols()), ~ sum(.)) %>% 
        left_join(tibble(group = rep(groups, each = days),
                         time = rep(c(1:days), length(groups)),
                         mean = mc[[element]][, var, ] %>% 
                           apply(MARGIN = 1, mean)) %>% 
                    bind_cols(mc[[element]][, var, ] %>% 
                                apply(MARGIN = 1, quantile, 
                                      c(0.5, 0.025, 0.975)) %>% 
                                t() %>% 
                                as_tibble() %>% 
                                rename(median = '50%', lcl = '2.5%', ucl = '97.5%')),
                  by = c('group', 'time'))
      
    } else {
      tibble(group = rep(groups, each = days),
             time = rep(c(1:days), length(groups)),
             mean = NA,
             median = NA,
             lcl = NA,
             ucl = NA)
    }
  } else if (by == 'year') {
    
    f = rep(groups, each = days) %>% as.factor()
    
    if (var %in% dimnames(mc[[element]])[[2]]) {
      
      origdat %>% select(group, total = !!var) %>% 
        mutate(habitat = var) %>% 
        group_by(group, habitat) %>% 
        summarize_at(vars(-group_cols()), ~ sum(.)) %>% 
        left_join(tibble(group = groups,
                         mc_mean = apply(mc[[element]][, var, ], 
                                         MARGIN = 2, split, f) %>% 
                           map_dfr(map, sum) %>% 
                           apply(MARGIN = 2, mean)) %>% 
                    bind_cols(apply(mc[[element]][, var, ], 
                                    MARGIN = 2, split, f) %>% 
                                map_dfr(map, sum) %>% 
                                apply(MARGIN = 2, quantile, 
                                      c(0.5, 0.025, 0.975)) %>% 
                                t() %>% as_tibble() %>% 
                                rename(mc_median = '50%', lcl = '2.5%', 
                                       ucl = '97.5%')),
                  by = 'group')
      
    } else if (var == 'wetlands') {

      origdat %>% select(group, total = !!var) %>% 
        mutate(habitat = var) %>% 
        group_by(group, habitat) %>% 
        summarize_at(vars(-group_cols()), ~ sum(.)) %>% 
        left_join(tibble(group = groups,
                         # first sum across perm and seas for each date, year, and iteration
                         mc_mean = apply(mc[[element]][, c('perm', 'seas'), ],
                                         MARGIN = c(1, 3), sum) %>% 
                           apply(MARGIN = 2, split, f) %>% 
                           map_dfr(map, sum) %>% 
                           apply(MARGIN = 2, mean)) %>% 
                    bind_cols(apply(mc[[element]][, c('perm', 'seas'), ],
                                    MARGIN = c(1, 3), sum) %>% 
                                apply(MARGIN = 2, split, f) %>% 
                                map_dfr(map, sum) %>% 
                                apply(MARGIN = 2, quantile, 
                                      c(0.5, 0.025, 0.975)) %>% 
                                t() %>% as_tibble() %>% 
                                rename(mc_median = '50%', lcl = '2.5%', 
                                       ucl = '97.5%')),
                  by = 'group')
      
    } else if (var == 'totalfree') {

      origdat %>% select(group, wetlands, rice, corn, other) %>% 
        mutate(habitat = var) %>% 
        group_by(group, habitat) %>% 
        summarize_at(vars(-group_cols()), ~ sum(.)) %>% 
        mutate(total = wetlands + rice + corn + other) %>% 
        select(group, habitat, total) %>% 
        left_join(tibble(group = groups,
                         # first sum across unincentivized habitat for each date, year, and iteration
                         mc_mean = apply(mc[[element]][, c('perm', 'seas', 'rice', 'corn', 'other'), ],
                                         MARGIN = c(1, 3), sum) %>% 
                           apply(MARGIN = 2, split, f) %>% 
                           map_dfr(map, sum) %>% 
                           apply(MARGIN = 2, mean)) %>% 
                    bind_cols(apply(mc[[element]][, c('perm', 'seas', 'rice', 'corn', 'other'), ],
                                    MARGIN = c(1, 3), sum) %>% 
                                apply(MARGIN = 2, split, f) %>% 
                                map_dfr(map, sum) %>% 
                                apply(MARGIN = 2, quantile, 
                                      c(0.5, 0.025, 0.975)) %>% 
                                t() %>% as_tibble() %>% 
                                rename(mc_median = '50%', lcl = '2.5%', 
                                       ucl = '97.5%')),
                  by = 'group')
      
    } else if (var == 'grandtotal') {
      
      origdat %>% select(-time, -watertype) %>% 
        mutate(habitat = var) %>% 
        group_by(group, habitat) %>% 
        summarize_at(vars(-group_cols()), ~ sum(.)) %>% 
        mutate(total = wetlands + rice + corn + other + incentives) %>% 
        select(group, habitat, total) %>% 
        left_join(tibble(group = groups,
                         # first sum across unincentivized habitat for each date, year, and iteration
                         mc_mean = apply(
                           mc[[element]][, c('perm', 'seas', 'rice', 'corn', 
                                             'other', 'incentives'), ],
                           MARGIN = c(1, 3), sum) %>% 
                           apply(MARGIN = 2, split, f) %>% 
                           map_dfr(map, sum) %>% 
                           apply(MARGIN = 2, mean)) %>% 
                    bind_cols(
                      apply(mc[[element]][, c('perm', 'seas', 'rice', 'corn', 
                                              'other', 'incentives'), ],
                            MARGIN = c(1, 3), sum) %>% 
                        apply(MARGIN = 2, split, f) %>% 
                        map_dfr(map, sum) %>% 
                        apply(MARGIN = 2, quantile, c(0.5, 0.025, 0.975)) %>% 
                        t() %>% as_tibble() %>% 
                        rename(mc_median = '50%', lcl = '2.5%', ucl = '97.5%')
                      ),
                  by = 'group')
      
      
    } else {
      tibble(group = rep(groups, each = days),
             mean = NA,
             median = NA,
             lcl = NA,
             ucl = NA)
    }
  } else if (by == 'season') {
    # season and year totals
    f = rep(groups, each = days) %>% as.factor()
    g = rep(c(rep('fall', 184), rep('spring', days - 184)), length(groups))
    fg = paste(f, g) %>% as.factor()
    
    if (var %in% dimnames(mc[[element]])[[2]]) {
      origdat %>% select(group, !!var) %>% mutate(season = g) %>% 
        group_by(group, season) %>% 
        summarize_at(vars(-group_cols()), ~ sum(.)) %>% 
        rename(total = shortfall) %>% 
        left_join(enframe(apply(mc[[element]][, var, ], 
                                MARGIN = 2, split, fg) %>% 
                            map_dfr(map, sum) %>% 
                            apply(MARGIN = 2, mean)) %>% 
                    left_join(apply(mc[[element]][, var, ], 
                                    MARGIN = 2, split, fg) %>% 
                                map_dfr(map, sum) %>% 
                                apply(MARGIN = 2, quantile, 
                                      c(0.5, 0.025, 0.975)) %>% 
                                t() %>% as.data.frame() %>% 
                                rownames_to_column() %>% 
                                rename(mc_median = '50%', lcl = '2.5%', 
                                       ucl = '97.5%'),
                              by = c('name' = 'rowname')) %>% 
                    separate(name, into = c('group', 'season'), sep = ' ') %>% 
                    rename(mc_mean = value),
                  by = c('group', 'season'))
    }
  } else {
    tibble(group = fg,
           mean = NA,
           median = NA,
           lcl = NA,
           ucl = NA) %>% 
      separate(group, into = c('group', 'season'), sep = ' ')
  }

}
