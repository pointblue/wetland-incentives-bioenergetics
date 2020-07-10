# # README---------------
# # Run bioenergetics models


fit_bioenergetics_model <- function(needspath, energypath, habitatchange) {
  needs <- read_csv(needspath, col_types = cols())
  energydens <- read_csv(energypath, col_types = cols()) %>%
    filter(habitat != 'corn') %>%
    mutate(habitat = recode(habitat, corn_north = 'corn'),
           habitat = factor(habitat, levels = c('perm', 'seas', 'rice', 'corn', 
                                                'other', 'whep_vardd', 'whep_fall', 
                                                'br_spring', 'br_fall')))  
  # add missing values for br and whep equivalent to rice:
  energydens <- energydens %>% 
    complete(habitat, 
             fill = list(value = energydens$value[energydens$habitat == 'rice'], 
                         lcl = energydens$lcl[energydens$habitat == 'rice'], 
                         ucl = energydens$ucl[energydens$habitat == 'rice']))
  
  
  # 4 models: with/without incentives; for baseline popsize and for pop
  # objectives; each run for all years in habitatchange
  res <- list(
    ## with population objectives:
    map(habitatchange, 
        ~ bioenergmod::run_bioenergmod_loop(
          habitat.available = .x$openwater,
          habitat.accessible = .x$accessible,
          habitat.added = .x$added,
          habitat.returned = .x$returned,
          prop.accessible = .x$prop.accessible,
          energyneed = needs$DER.obj,
          energydens = energydens)),
    
    ## with baseline/"current" population size:
    map(habitatchange, 
        ~ bioenergmod::run_bioenergmod_loop(
          habitat.available = .x$openwater,
          habitat.accessible = .x$accessible,
          habitat.added = .x$added,
          habitat.returned = .x$returned,
          prop.accessible = .x$prop.accessible,
          energyneed = needs$DER.obs,
          energydens = energydens)),
    
    ## repeat without incentive acres:
    ## - with population objectives:
    map(habitatchange, 
        ~ bioenergmod::run_bioenergmod_loop(
          habitat.available = .x$openwater %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          habitat.accessible = .x$accessible %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          habitat.added = .x$added %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          habitat.returned = .x$returned %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          prop.accessible = .x$prop.accessible %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          energyneed = needs$DER.obj,
          energydens = energydens)),
    
    ## - with baseline/"current" population size:
    map(habitatchange, 
        ~ bioenergmod::run_bioenergmod_loop(
          habitat.available = .x$openwater %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          habitat.accessible = .x$accessible %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          habitat.added = .x$added %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          habitat.returned = .x$returned %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          prop.accessible = .x$prop.accessible %>%
            select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
          energyneed = needs$DER.obs,
          energydens = energydens)))
  
  names(res) <- c('obj_with', 'obs_with', 'obj_free', 'obs_free')
  return(res)
  
}

