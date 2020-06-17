bioenergmod_mc_custom <- function(nsim, energyneed, tothabitat, energysim,
                                  floodsim, accessiblesim, wetsplit = TRUE,
                                  mintime, maxtime, grp, addon = NULL) {

  pb <- progress_estimated(nsim)
  
  results <- map(c(1:nsim), change_calc, 
                 grp = grp, floodsim = floodsim, depthsim = depthsim,
                 mintime = mintime, maxtime = maxtime, addon = addon, 
                 energyneed = energyneed, tothabitat = tothabitat,
                 energysim = energysim, .pb = pb)

  # compile results across all simulations into common arrays
  final = list()
  final$energy = map(results, function(x) {x[["energy"]]}) %>% 
    abind::abind(along = 3)
  final$energy.supply = map(results, function(x) {x[["energy.supply"]]}) %>% 
    abind::abind(along = 3)
  final$energy.accessible = map(results, function(x) {x[["energy.accessible"]]}) %>% 
    abind::abind(along = 3)
  final$energy.consumed = map(results, function(x) {x[["energy.consumed"]]}) %>% 
    abind::abind(along = 3)
  final$energy.lost = map(results, function(x) {x[["energy.lost"]]}) %>% 
    abind::abind(along = 3)
  
  return(final)
  
}

change_calc = function(i, grp, floodsim, depthsim, mintime, maxtime, br, whep, 
                       energyneed, tothabitat, energysim, addon = addon,
                       .pb = NULL) {
  
  # for each simulation i, calculate habitat change in each year from wetlands 
  #   and unincentivized crops, then add specifics from incentive programs,
  #   and run bioenergetics model
  
  flood <- map_dfr(floodsim, function(x) {
    data.frame(yday = c(1:319), value = x[mintime:maxtime, i])
    }, .id = 'habitat')
  
  pp <- flood %>% filter(habitat == 'prop.perm') %>%
    mutate(habitat = recode(habitat, prop.perm = 'wetlands')) %>%
    rename(prop.perm = value)
  
  flood <- flood %>% filter(habitat != 'prop.perm') %>%
    full_join(pp, by = c('habitat', 'yday'))
  
  accessible <- map_dfr(depthsim, function(x) {
    if (is.null(dim(x))) {
      data.frame(yday = c(1:319),
                 value = x[i])
    } else {
      data.frame(yday = c(1:319),
                 value = x[, i])
    }
  }, .id = 'habitat')
  
  change <- bioenergmod::calculate_habitat_change(
    tothabitat = tothabitat, flood = flood, accessible = accessible,
    time = 'yday', value = 'value', wetsplit = TRUE)
  
  # if they exist, add explicit values from additional data in "addon"
  if (!is.null(addon)) {
    change$openwater <- change$openwater %>%
      full_join(addon %>% filter(group == grp) %>% select(habitat, time = yday, available) %>% 
                  spread(key = habitat, value = available),
                by = c('time'))
    
    change$accessible <- change$accessible %>%
      full_join(addon %>% filter(group == grp) %>% select(habitat, time = yday, accessible) %>% 
                  spread(key = habitat, value = accessible),
                by = c('time'))
    
    change$added <- change$added %>%
      full_join(addon %>% filter(group == grp) %>% select(habitat, time = yday, added) %>% 
                  spread(key = habitat, value = added),
                by = c('time'))
    
    change$returned <- change$returned %>%
      full_join(addon %>% filter(group == grp) %>% select(habitat, time = yday, returned) %>% 
                  spread(key = habitat, value = returned),
                by = c('time'))
    
    change$prop.accessible <- change$prop.accessible %>% 
      full_join(addon %>% filter(group == grp) %>% select(habitat, time = yday, prop.accessible) %>%
                  spread(key = habitat, value = prop.accessible),
                by = c('time'))
  }
  
  energydens <- map_dfr(energysim, function(x) {
    data.frame(value = x[i])
    }, .id = 'habitat')
  
  model <- run_bioenergmod_loop(energyneed = energyneed, 
                                energydens = energydens,
                                habitat.available = change$openwater,
                                habitat.accessible = change$accessible,
                                habitat.added = change$added,
                                habitat.returned = change$returned,
                                prop.accessible = change$prop.accessible)
  
  # update progress bar:
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) {
    .pb$tick()$print()
  }
  
  return(model)
}
