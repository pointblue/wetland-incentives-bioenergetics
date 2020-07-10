
fill_all_shortfalls = function(needspath, 
                               population = c('baseline', 'objectives'),
                               energypath, newhabitatenergy = 'rice',
                               orighabitatchange, 
                               group = c('2013-14', '2014-15', '2015-16', '2016-17'), 
                               drawdown = 14, newhabitatpath, limit = 100000) {
  
  framework <- read_csv(newhabitatpath, col_types = cols()) %>% 
    select(start, end) %>% 
    mutate(filled = 0)
  
  df <- expand.grid(group = group, population = population) %>% 
    mutate_all(as.character)
  
  map2_dfr(.x = df$group %>% set_names(paste(df$group, df$population, sep="_")), 
           .y = df$population,
           ~ fill_shortfalls(needspath = needspath, 
                             population = .y, 
                             energypath = energypath, 
                             newhabitatenergy = newhabitatenergy,
                             orighabitatchange = orighabitatchange, 
                             group = .x, 
                             drawdown = drawdown, 
                             framework = framework, 
                             limit = limit),
           .id = 'label')
}

fill_shortfalls = function(needspath, population, energypath, newhabitatenergy,
                           orighabitatchange, group, drawdown,
                           framework, limit) {
  
  # pull correct daily energy requirements
  needs <- read_csv(needspath, col_types = cols()) %>% 
    mutate(need = case_when(population == 'objectives' ~ DER.obj, 
                            population == 'baseline' ~ DER.obs)) %>% 
    pull(need)
  
  # assign energy density to new habitat
  energydens <- read_csv(energypath, col_types = cols()) %>%
    filter(habitat != 'corn') %>%
    mutate(habitat = recode(habitat, corn_north = 'corn'))
  energydens_new <- bind_rows(energydens, 
                              energydens %>% 
                                filter(habitat == ifelse(newhabitatenergy == 'wetlands', 
                                                         'seas', 
                                                         newhabitatenergy)) %>% 
                                mutate(habitat = 'new')) %>% 
    mutate(habitat = factor(habitat, levels = c('perm', 'seas', 'rice', 'corn', 
                                                'other', 'new')))
  
  # pull correct group (year)
  origchange = orighabitatchange[[group]]

    # loop through start dates, finding the next amount of habitat needed to fill
  # shortfalls
  cat('Starting', group, 'with', population, 'population:\n')
  for (t in c(1:length(framework$start))) {
    cat('t = ', t, '\n')
    next_fill = fill_next_shortfall(framework = framework, 
                                    t = t, 
                                    origchange = origchange, 
                                    drawdown = drawdown,
                                    energyneed = needs,
                                    energydens = energydens_new,
                                    limit = limit)
    framework$filled[t] = next_fill
  }
  return(framework)
}
  

# experimental function to automatically find the next amount of habitat needed
# to fill shortfalls
fill_next_shortfall <- function(framework, t, origchange, drawdown, 
                                energyneed, energydens, limit) {
  # try zero first
  framework$filled[t] = 0
  change_new <- update_habitat_change2(origchange, framework, column = 'filled', 
                                       drawdown = drawdown)
  mod <- bioenergmod::run_bioenergmod_loop(
    habitat.available = change_new$openwater %>%
      select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
    habitat.accessible = change_new$accessible %>%
      select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
    habitat.added = change_new$added %>%
      select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
    habitat.returned = change_new$returned %>%
      select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
    prop.accessible = change_new$prop.accessible %>%
      select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
    energyneed = energyneed,
    energydens = energydens)
  
  short <- calculate_shortfall(mod, t, framework)
  
  if (short == 0) {
    cat('No additional habitat needed.\n')
    final = 0
  } else {
    # continue on with finding how much is needed
    options1 = c(5000, seq(10000, limit, by = 10000))
    # round 1: proceed in order, stop when shortfalls = 0
    for (i in c(1:length(options1))) {
      # add habitat to next time interval, update habitat change data, run model,
      # find total shortfall up to the next start date
      framework$filled[t] = options1[i]
      change_new <- update_habitat_change2(origchange, framework, column = 'filled', 
                                           drawdown = drawdown)
      mod <- bioenergmod::run_bioenergmod_loop(
        habitat.available = change_new$openwater %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.accessible = change_new$accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.added = change_new$added %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.returned = change_new$returned %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        prop.accessible = change_new$prop.accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        energyneed = energyneed,
        energydens = energydens)
      
      short <- calculate_shortfall(mod, t, framework)
      short_after <- calculate_shortfall_after(mod, t, framework)
      short_during <- calculate_shortfall_during(mod, t, framework)
      
      if (short == 0 & (short_after > 0 | short_during == 0)) { 
          bracket1_high = options1[i]
          if (i == 1) {bracket1_low = 0} else {bracket1_low = options1[i-1]}
          # cat('Round 1 between: ', bracket1_low, 'and', bracket1_high, '\n')
          break 
      } else if (i == length(options1)) {
        stop('Adding', limit, 'ha was not sufficient.')
      }
    }
    
    # round 2: look within round 1 bracket
    if (bracket1_high <= 10000) {
      options2 = seq(bracket1_low, bracket1_high, by = 1000)
    } else {
      options2 = seq(bracket1_low, bracket1_high, by = 2000)
    }
    options2 = options2[-which(options2 %in% c(bracket1_low, bracket1_high))]
    for (j in c(1:length(options2))) {
      # add habitat to next time interval, update habitat change data, run model,
      # find total shortfall up to the next start date
      framework$filled[t] = options2[j]
      change_new <- update_habitat_change2(origchange, framework, column = 'filled', 
                                           drawdown = drawdown)
      mod <- bioenergmod::run_bioenergmod_loop(
        habitat.available = change_new$openwater %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.accessible = change_new$accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.added = change_new$added %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.returned = change_new$returned %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        prop.accessible = change_new$prop.accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        energyneed = energyneed,
        energydens = energydens)
      
      short <- calculate_shortfall(mod, t, framework)
      short_after <- calculate_shortfall_after(mod, t, framework)
      short_during <- calculate_shortfall_during(mod, t, framework)
      
      if (short == 0 & (short_after > 0 | short_during == 0)) {
          bracket2_high = options2[j]
          if (j == 1) {bracket2_low = bracket1_low} else {bracket2_low = options2[j-1]}
          # cat('Round 2 between: ', bracket2_low, 'and', bracket2_high, '\n')
          break 
      } else if (j == length(options2)) {
          bracket2_high = bracket1_high
          bracket2_low = options2[j]
          # cat('Round 2 between: ', bracket2_low, 'and', bracket2_high, '\n')
      }
    }

    # round 3: look within round 2 bracket
    options3 = seq(bracket2_low, bracket2_high, by = 250)
    options3 = options3[-which(options3 %in% c(bracket2_low, bracket2_high))]
    for (k in c(1:length(options3))) {
      # add habitat to next time interval, update habitat change data, run model,
      # find total shortfall up to the next start date
      framework$filled[t] = options3[k]
      change_new <- update_habitat_change2(origchange, framework, column = 'filled', 
                                           drawdown = drawdown)
      mod <- bioenergmod::run_bioenergmod_loop(
        habitat.available = change_new$openwater %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.accessible = change_new$accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.added = change_new$added %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.returned = change_new$returned %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        prop.accessible = change_new$prop.accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        energyneed = energyneed,
        energydens = energydens)
      
      short <- calculate_shortfall(mod, t, framework)
      short_after <- calculate_shortfall_after(mod, t, framework)
      short_during <- calculate_shortfall_during(mod, t, framework)
      
      if (short == 0 & (short_after > 0 | short_during == 0)) {
        bracket3_high = options3[k]
        if (k == 1) {bracket3_low = bracket2_low} else {bracket3_low = options3[k-1]}
        # cat('Round 3 between: ', bracket3_low, 'and', bracket3_high, '\n')
        break 
      } else if (k == length(options3)) {
          bracket3_high = bracket2_high
          bracket3_low = options3[k]
          # cat('Round 3 between: ', bracket3_low, 'and', bracket3_high, '\n')
      }
    }
  
    # round 4: look within round 3 bracket
    options4 = seq(bracket3_low, bracket3_high, by = 50)
    options4 = options4[-which(options4 %in% c(bracket3_low, bracket3_high))]
    for (l in c(1:length(options4))) {
      # add habitat to next time interval, update habitat change data, run model,
      # find total shortfall up to the next start date
      framework$filled[t] = options4[l]
      change_new <- update_habitat_change2(origchange, framework, column = 'filled', 
                                           drawdown = drawdown)
      mod <- bioenergmod::run_bioenergmod_loop(
        habitat.available = change_new$openwater %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.accessible = change_new$accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.added = change_new$added %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        habitat.returned = change_new$returned %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        prop.accessible = change_new$prop.accessible %>%
          select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
        energyneed = energyneed,
        energydens = energydens)
      
      short <- calculate_shortfall(mod, t, framework)
      short_after <- calculate_shortfall_after(mod, t, framework)
      short_during <- calculate_shortfall_during(mod, t, framework)

      if (short == 0 & (short_after > 0 | short_during == 0)) {
        cat('Final value: ', options4[l], '\n')
        final = options4[l]
        break 
      } else if (l == length(options4)) {
        # nothing in round 4 worked, so bracket3_high was the minimum sufficient value
        final = bracket3_high
        cat('Final value: ', final, '\n')
      }
    }
  }
  return(final)
}

# calculate shortfall up through the start of the next time interval
calculate_shortfall = function(mod, t, framework) {
  mod$energy %>% 
    filter(time < ifelse(t < length(framework$start), 
                         framework$start[t+1],
                         max(framework$end))) %>% 
    pull(shortfall) %>% 
    sum()
}
# also check for any after end date and within season
calculate_shortfall_after = function(mod, t, framework) {
  if (framework$start[t] < 185) {
    mod$energy %>% 
      filter(shortfall > 0 & time < 185 & time > framework$end[t]) %>% 
      pull(shortfall) %>% sum()
  } else {
    mod$energy %>% 
      filter(shortfall > 0 & time > framework$end[t]) %>% 
      pull(shortfall) %>% sum()
  }
}
# check for any remaining shortfalls before end date
calculate_shortfall_during = function(mod, t, framework) {
  mod$energy %>% 
    filter(shortfall > 0 & 
             time >= ifelse(t < length(framework$start),
                            framework$start[t+1],
                            framework$start[t]) & 
             time <= framework$end[t]) %>% 
    pull(shortfall) %>% sum()
}


test_added_habitat = function(needspath, energypath, newhabitatenergy = 'rice',
                              filledhabitat, orighabitatchange, drawdown = 14) {
  
  needs <- read_csv(needspath, col_types = cols())
  
  energydens <- read_csv(energypath, col_types = cols()) %>%
    filter(habitat != 'corn') %>%
    mutate(habitat = recode(habitat, corn_north = 'corn'))
  energydens_new <- bind_rows(energydens, 
                              energydens %>% 
                                filter(habitat == ifelse(newhabitatenergy == 'wetlands', 
                                                         'seas', 
                                                         newhabitatenergy)) %>% 
                                mutate(habitat = 'new')) %>% 
    mutate(habitat = factor(habitat, levels = c('perm', 'seas', 'rice', 'corn', 
                                                'other', 'new')))
  
  change_obj <- update_habitat_change(orighabitatchange, filledhabitat, 
                                      column = 'objectives', 
                                      drawdown = drawdown)
  change_obs <- update_habitat_change(orighabitatchange, filledhabitat, 
                                      column = 'baseline', 
                                      drawdown = drawdown)
  
  res = list(
    #objectives
    map(change_obj, 
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
          energydens = energydens_new)),
    #baseline
    map(change_obs, 
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
          energydens = energydens_new)))
  names(res) <- c('obj_free', 'obs_free')
  
  obj_short <- map(res$obj_free, function(x) {
    x$energy %>% filter(shortfall > 0) %>% select(time, shortfall)
  })
  obj_l <- map(obj_short, nrow)
  
  if (any(obj_l > 0)) {
    cat('\n\nObjectives: shortfalls remain\n\n')
    print(map_dfr(obj_short, function(x) {x %>% head()}, .id = 'group'))
  } else {
    cat('\n\nObjectives: no shortfalls remain')
  }
  
  obs_short <- map(res$obs_free, function(x) {
    x$energy %>% filter(shortfall > 0) %>% select(time, shortfall)
  })
  obs_l <- map(obs_short, nrow)
  if (any(obs_l > 0)) {
    cat('\n\nBaseline: shortfalls remain\n\n')
    print(map_dfr(obs_short, function(x) {x %>% head()}, .id = 'group'))
  } else {
    cat('\n\nBaseline: no shortfalls remain')
  }
  return(res)
}

update_habitat_change = function(original, filled, column, drawdown = 14) {
  filled <- filled %>% mutate(end = end + drawdown) %>% 
    select(group, start, end, new = !!column)
  
  # summarize total area filled per day
  long <- tibble(time = seq(min(filled$start), 
                            min(max(filled$end), 
                                max(original[[1]]$openwater$time)))) %>% 
    full_join(filled, by = character()) %>%  #cross-join
    filter(time >= start & time <= end) %>%  #then filter to match start and end times
    arrange(group, time, start, end) %>% 
    group_by(group, start, end) %>% 
    mutate(added = case_when(time == min(time) ~ new,
                             TRUE ~ 0),
           returned = case_when(time == max(time) ~ new,
                                TRUE ~ 0)) %>% 
    group_by(group, time) %>% 
    summarize(openwater = sum(new), 
              added = sum(added),
              returned = sum(returned),
              .groups = 'drop') %>% 
    mutate(accessible = openwater,
           prop.accessible = 1,
           habitat = 'new') %>% 
    pivot_longer(openwater:prop.accessible) %>% 
    mutate(name = factor(name, 
                         levels = c('openwater', 'added', 'returned', 
                                    'accessible', 'prop.accessible'))) %>% 
    pivot_wider(names_from = 'habitat', values_from = 'value') %>% 
    arrange(group, name, time) %>% 
    split(.$group) %>% 
    map(function(x) {
      x %>% select(-group) %>% as.data.frame() %>% split(.$name)
    })
  
  map2(original, long, map2, 
       function(x, y) {
         left_join(x, y %>% select(-name), by = 'time')
       }) 
}

# for when original is not a list of years
update_habitat_change2 = function(original, framework, column, drawdown = 14) {
  framework <- framework %>% mutate(end = end + drawdown) %>% 
    select(start, end, new = !!column)
  
  # summarize total area filled per day
  long <- tibble(time = seq(min(framework$start), 
                            min(max(framework$end), 
                                max(original$openwater$time)))) %>% 
    full_join(framework, by = character()) %>%  #cross-join
    filter(time >= start & time <= end) %>%  #then filter to match start and end times
    arrange(time, start, end) %>% 
    group_by(start, end) %>% 
    mutate(added = case_when(time == min(time) ~ new,
                             TRUE ~ 0),
           returned = case_when(time == max(time) ~ new,
                                TRUE ~ 0)) %>% 
    group_by(time) %>% 
    summarize(openwater = sum(new), 
              added = sum(added),
              returned = sum(returned),
              .groups = 'drop') %>% 
    mutate(accessible = openwater,
           prop.accessible = 1,
           habitat = 'new') %>% 
    pivot_longer(openwater:prop.accessible) %>% 
    mutate(name = factor(name, 
                         levels = c('openwater', 'added', 'returned', 
                                    'accessible', 'prop.accessible'))) %>% 
    pivot_wider(names_from = 'habitat', values_from = 'value') %>% 
    arrange(name, time) %>% 
    split(.$name)

  map2(original, long, 
       function(x, y) {
         left_join(x, y %>% select(-name), by = 'time')
       }) 
}


# fill_next_shortfall <- function(fill, t, orighabitatchange, drawdown, 
#                                 energyneed, energydens) {
#   # test 10k intervals: add options to next time interval, update habitat
#   # change data, run model, find total shortfall up to the next start date
#   options10k = c(0, 10000, 20000, 30000, 40000, 50000)  
#   test10k <- map_dbl(options10k, function(x) {
#       fill$filled[t] = x
#       change_new <- update_habitat_change2(origchange, fill, column = 'filled', 
#                                            drawdown = drawdown)
#       mod <- bioenergmod::run_bioenergmod_loop(
#             habitat.available = change_new$openwater %>%
#               select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#             habitat.accessible = change_new$accessible %>%
#               select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#             habitat.added = change_new$added %>%
#               select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#             habitat.returned = change_new$returned %>%
#               select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#             prop.accessible = change_new$prop.accessible %>%
#               select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#             energyneed = energyneed,
#             energydens = energydens)
#       mod$energy %>% 
#         filter(time < fill$start[t+1]-1) %>% 
#         summarize(shortfall = sum(shortfall)) %>% 
#         pull(shortfall)
#     })
#     
#   # determine which 10k options bracket the amount of habitat needed
#   if (any(test10k == 0)) {
#     bracket10k_high = options10k[min(which(test10k == 0))]
#     bracket10k_low = options10k[min(which(test10k == 0))-1]
#   } else {
#     stop('Adding 50,000 ha did not fill shortfall')
#   }
#   
#   # test 10k bracket by 2k intervals
#   options2k = seq(bracket10k_low, bracket10k_high, by = 2000)
#   options2k = options2k[-which(options2k %in% c(bracket10k_low, bracket10k_high))]
#   test2k <- map_dbl(options2k, function(x) {
#     fill$filled[t] = x
#     change_new <- update_habitat_change2(origchange, fill, column = 'filled', 
#                                          drawdown = drawdown)
#     mod <- bioenergmod::run_bioenergmod_loop(
#       habitat.available = change_new$openwater %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.accessible = change_new$accessible %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.added = change_new$added %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.returned = change_new$returned %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       prop.accessible = change_new$prop.accessible %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       energyneed = energyneed,
#       energydens = energydens)
#     mod$energy %>% 
#       filter(time < fill$start[t+1]-1) %>% 
#       summarize(shortfall = sum(shortfall)) %>% 
#       pull(shortfall)
#   })
#   if (all(test2k == 0)) {
#     bracket2k_high = min(options2k)
#     bracket2k_low = bracket10k_low
#   } else if (any(test2k == 0)) {
#     bracket2k_high = options2k[min(which(test2k == 0))]
#     bracket2k_low = options2k[min(which(test2k == 0))-1]
#   } else {
#     bracket2k_high = bracket10k_high
#     bracket2k_low = max(options2k)
#   }
#   
#   # test 2k bracket by intervals of 250
#   options250 = seq(bracket2k_low, bracket2k_high, by = 250)
#   options250 = options250[-which(options250 %in% c(bracket2k_low, bracket2k_high))]
#   test250 <- map_dbl(options250, function(x) {
#     fill$filled[t] = x
#     change_new <- update_habitat_change2(origchange, fill, column = 'filled', 
#                                          drawdown = drawdown)
#     mod <- bioenergmod::run_bioenergmod_loop(
#       habitat.available = change_new$openwater %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.accessible = change_new$accessible %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.added = change_new$added %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.returned = change_new$returned %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       prop.accessible = change_new$prop.accessible %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       energyneed = energyneed,
#       energydens = energydens)
#     mod$energy %>% 
#       filter(time < fill$start[t+1]-1) %>% 
#       summarize(shortfall = sum(shortfall)) %>% 
#       pull(shortfall)
#   })
#   if (all(test250 == 0)) {
#     bracket250_high = min(options250)
#     bracket250_low = bracket2k_low
#   } else if (any(test250 == 0)) {
#     bracket250_high = options250[min(which(test250 == 0))]
#     bracket250_low = options250[min(which(test250 == 0))-1]
#   } else {
#     bracket250_high = bracket2k_high
#     bracket250_low = max(options250)
#   }
#   
#   # test 250 bracket by intervals of 50
#   options50 = seq(bracket250_low, bracket250_high, by = 50)
#   options50 = options50[-which(options50 %in% c(bracket250_low, bracket250_high))]
#   test50 <- map_dbl(options50, function(x) {
#     fill$filled[t] = x
#     change_new <- update_habitat_change2(origchange, fill, column = 'filled', 
#                                          drawdown = drawdown)
#     mod <- bioenergmod::run_bioenergmod_loop(
#       habitat.available = change_new$openwater %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.accessible = change_new$accessible %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.added = change_new$added %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       habitat.returned = change_new$returned %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       prop.accessible = change_new$prop.accessible %>%
#         select(-br_fall, -br_spring, -whep_vardd, -whep_fall),
#       energyneed = energyneed,
#       energydens = energydens)
#     mod$energy %>% 
#       filter(time < fill$start[t+1]-1) %>% 
#       summarize(shortfall = sum(shortfall)) %>% 
#       pull(shortfall)
#   })
#   if (all(test50 == 0)) {
#     final = min(options50)
#   } else if (any(test50 == 0)) {
#     final = options50[min(which(test50 == 0))]
#   } else {
#     final = bracket250_high
#   }
#   return(final)
# }