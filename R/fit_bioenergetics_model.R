# # README---------------
# # Run bioenergetics models
# #
# # PACKAGES
# library(tidyverse)
# library(bioenergmod) # devtools::install_github("kdybala/bioenergmod")
# library(patchwork)
# 
# # INPUTS
# objectives <- 'data/cvjv_orig/daily_energy_requirement.csv'
# energy_density <- 'data/cvjv_orig/energy_content.csv'
# habitat_change <- 'output/habitat_change.RData'
# 
# # OUTPUTS
# bioenerg_results <- 'output/bioenergetics_results.RData'
# results_energy <- 'output/bioenergetics_results_energy.csv'
# results_energy_accessible <- 'output/bioenergetics_results_energy_accessible.csv'
# results_energy_consumed <- 'output/bioenergetics_results_energy_consumed.csv'
# results_energy_lost <- 'output/bioenergetics_results_energy_lost.csv'
# 
# 

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

# Notes on energy needs data: 
# - observed = baseline/"current" estimated population size
# - obj = long-term population objective (~double baseline/current)
# - mass.kg = estimated average body mass per individual depending on seasonally-
#    varying species compositions (used for estimating daily energy requirement)
# - DER.obj = estimated Daily Energy Requirement for the long-term population 
#    objective, including correction for assimilation efficiency and an 
#    adjustment upward by 33% in the spring for migration
# - DER.obj2 = DER for population objective without spring adjustment
# - DER.obs = DER for baseline/"current" population size with spring adjustment
# - DER.obs2 = DER for baseline/"current" population size without spring adjustment




# BASIC DETERMINISTIC MODEL-------------------

# ## repeat with only fall incentive acres:
# ## - with population objectives:
# obj_det3 <- map(change_all, 
#                 ~ run_bioenergmod_loop(habitat.available = .x$openwater %>%
#                                          select(-br_spring, -whep_vardd),
#                                        habitat.accessible = .x$accessible %>%
#                                          select(-br_spring, -whep_vardd),
#                                        habitat.added = .x$added %>%
#                                          select(-br_spring, -whep_vardd),
#                                        habitat.returned = .x$returned %>%
#                                          select(-br_spring, -whep_vardd),
#                                        prop.accessible = .x$prop.accessible %>%
#                                          select(-br_spring, -whep_vardd),
#                                        energyneed = needs$DER.obj,
#                                        energydens = energydens))
# 
# ## - with baseline/"current" population size:
# obs_det3 <- map(change_all, 
#                 ~ run_bioenergmod_loop(habitat.available = .x$openwater %>%
#                                          select(-br_spring, -whep_vardd),
#                                        habitat.accessible = .x$accessible %>%
#                                          select(-br_spring, -whep_vardd),
#                                        habitat.added = .x$added %>%
#                                          select(-br_spring, -whep_vardd),
#                                        habitat.returned = .x$returned %>%
#                                          select(-br_spring, -whep_vardd),
#                                        prop.accessible = .x$prop.accessible %>%
#                                          select(-br_spring, -whep_vardd),
#                                        energyneed = needs$DER.obs,
#                                        energydens = energydens))

# 
# # results <- list(obj_det = obj_det, obj_det2 = obj_det2, obj_det3 = obj_det3,
# #                 obs_det = obs_det, obs_det2 = obs_det2, obs_det3 = obs_det3)
# results <- list(obj_det = obj_det, obj_det2 = obj_det2,
#                 obs_det = obs_det, obs_det2 = obs_det2)
# 
# save(results, file = here::here(bioenerg_results))
# 
# 
# # RESULTS EXTRACTION-----------
# # key <- tibble(scenario = c('obj_det', 'obj_det2', 'obj_det3', 'obs_det', 'obs_det2', 'obs_det3'),
# #               incentives = c('all', 'none', 'fall only', 'all', 'none', 'fall only'),
# #               population = c(rep('objectives', 3), rep('baseline', 3)))
# key <- tibble(scenario = c('obj_det', 'obj_det2', 'obs_det', 'obs_det2'),
#               incentives = c('all', 'none', 'all', 'none'),
#               population = c(rep('objectives', 2), rep('baseline', 2)))
# 
# energysum <- map_dfr(results, 
#                      ~map_dfr(.x, ~.x[['energy']], .id = 'group'), 
#                      .id = 'scenario') %>%
#   left_join(key, by = 'scenario')
# write_csv(energysum, here::here(results_energy))
# 
# accessible <- map_dfr(results, 
#                       ~map_dfr(.x, ~.x[['energy.accessible']], .id = 'group'), 
#                       .id = 'scenario') %>%
#   left_join(key, by = 'scenario')
# write_csv(accessible, here::here(results_energy_accessible))
# 
# consumed <- map_dfr(results, 
#                     ~map_dfr(.x, ~.x[['energy.consumed']], .id = 'group'), 
#                     .id = 'scenario') %>%
#   left_join(key, by = 'scenario')
# write_csv(consumed, here::here(results_energy_consumed))
# 
# lost <- map_dfr(results, 
#                 ~map_dfr(.x, ~.x[['energy.lost']], .id = 'group'), 
#                 .id = 'scenario') %>%
#   left_join(key, by = 'scenario')
# write_csv(lost, here::here(results_energy_lost))
# 
# 
# # FILL SHORTFALLS ---------------------------------------------------------
# # only for scenario without incentives
# 
# # add habitat in one-month (28 day?) stretches; possibly staggered by 2 wks
# # assume 100% open water and accessible on day 1
# # assume 2 weeks to dry out (6 wks total)
# # 
# 
# update_habitat_change = function(original, filled, drawdown = 14) {
#   filled <- filled %>% mutate(end = end + drawdown) %>% 
#     select(group, start, end, filled)
#   # summarize total area filled per day
#   long <- tibble(time = seq(min(filled$start), 
#                             min(max(filled$end), 
#                                 max(original[[1]]$openwater$time)))) %>% 
#     full_join(filled, by = character()) %>% 
#     filter(time >= start & time <= end) %>% 
#     group_by(group, start, end) %>% 
#     mutate(added = case_when(time == min(time) ~ filled,
#                              TRUE ~ 0),
#            returned = case_when(time == max(time) ~ filled,
#                                 TRUE ~ 0)) %>% 
#     group_by(group, time) %>% 
#     summarize(openwater = sum(filled), 
#               added = sum(added),
#               returned = sum(returned),
#               .groups = 'drop') %>% 
#     mutate(accessible = openwater,
#            prop.accessible = 1,
#            habitat = 'filled') %>% 
#     pivot_longer(openwater:prop.accessible) %>% 
#     mutate(name = factor(name, levels = c('openwater', 'added', 'returned', 'accessible', 'prop.accessible'))) %>% 
#     pivot_wider(names_from = 'habitat', values_from = 'value') %>% 
#     arrange(name, time) %>% 
#     split(.$group) %>% 
#     map(function(x) {
#       x %>% select(-group) %>% as.data.frame() %>% split(.$name)
#     })
#   
#   map2(original, long, map2, 
#        function(x, y) {
#          left_join(x, y %>% select(-name), by = 'time')
#        }) 
# }
# 
# # assume all new acres are in rice/fallow fields
# energydens_new <- bind_rows(energydens, 
#                             energydens %>% filter(habitat == 'seas') %>% 
#                               mutate(habitat = recode(habitat, seas = 'filled')))
# 
# # 2013-14 shortfalls: day 9-79; 221-296
# fill <- readxl::read_excel('data/filling_shortfalls.xlsx', 
#                            sheet = 'obs_wetland')
# 
# change_new <- update_habitat_change(change_all, fill, drawdown = 14)
# 
# results <- map(change_new, 
#                ~ run_bioenergmod_loop(habitat.available = .x$openwater %>%
#                                         select(-br_fall, -br_spring, 
#                                                -whep_vardd, -whep_fall),
#                                       habitat.accessible = .x$accessible %>%
#                                         select(-br_fall, -br_spring, 
#                                                -whep_vardd, -whep_fall),
#                                       habitat.added = .x$added %>%
#                                         select(-br_fall, -br_spring,
#                                                -whep_vardd, -whep_fall),
#                                       habitat.returned = .x$returned %>%
#                                         select(-br_fall, -br_spring, 
#                                                -whep_vardd, -whep_fall),
#                                       prop.accessible = .x$prop.accessible %>%
#                                         select(-br_fall, -br_spring, 
#                                                -whep_vardd, -whep_fall),
#                                       energyneed = needs$DER.obs,
#                                       energydens = energydens_new))
# 
# tmax = 319
# p1 <- map_df(results, function(x) {x$energy}, .id = 'group') %>% 
#   select(group, time, shortfall) %>%
#   ggplot(aes(time, shortfall/1000000, color = group)) + geom_line() +
#   scale_x_continuous(limits = c(1, tmax))
# 
# p2 <- map_df(results, function(x) {x$energy.supply}, .id = 'group') %>% 
#   select(group, time, filled) %>%
#   ggplot(aes(time, filled/1000000, color = group)) + geom_line() +
#   scale_x_continuous(limits = c(1, tmax))
# 
# p1/p2
# 
# map_df(results, function(x) {x$energy}, .id = 'group') %>% 
#   select(group, time, shortfall) %>%
#   filter(time <= tmax & shortfall > 0 & group == '2013-14')
# map_df(results, function(x) {x$energy}, .id = 'group') %>% 
#   select(group, time, shortfall) %>%
#   filter(time <= tmax & shortfall > 0 & group == '2014-15')
# map_df(results, function(x) {x$energy}, .id = 'group') %>% 
#   select(group, time, shortfall) %>%
#   filter(time <= tmax & shortfall > 0 & group == '2015-16')
# map_df(results, function(x) {x$energy}, .id = 'group') %>% 
#   select(group, time, shortfall) %>%
#   filter(time <= tmax & shortfall > 0 & group == '2016-17')
