# README---------------
# Run bioenergetics models
#
# PACKAGES
library(tidyverse)
library(bioenergmod) # devtools::install_github("kdybala/bioenergmod")

# INPUTS
objectives <- 'data/cvjv_orig/daily_energy_requirement.csv'
energy_density <- 'data/cvjv_orig/energy_content.csv'
habitat_change <- 'output/habitat_change.RData'

# OUTPUTS
bioenerg_results <- 'output/bioenergetics_results.RData'
results_energy <- 'output/bioenergetics_results_energy.csv'
results_energy_accessible <- 'output/bioenergetics_results_energy_accessible.csv'
results_energy_consumed <- 'output/bioenergetics_results_energy_consumed.csv'
results_energy_lost <- 'output/bioenergetics_results_energy_lost.csv'


# ENERGY NEEDS------------

needs <- read_csv(here::here(objectives), col_types = cols())

# Note: 
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

ggplot(needs, aes(yday)) + 
  geom_line(aes(y = DER.obj/1000000)) + 
  geom_line(aes(y = DER.obj2/1000000), linetype = 'dashed') +
  geom_line(aes(y = DER.obs/1000000), color = 'red') + 
  geom_line(aes(y = DER.obs2/1000000), color = 'red', linetype = 'dashed') +
  ylab('Daily energy requirement (kJ, millions)')

# ENERGY DENSITY------------

energydens <- read_csv(here::here(energy_density), col_types = cols()) %>%
  filter(habitat != 'corn') %>%
  mutate(habitat = recode(habitat, corn_north = 'corn'),
         habitat = factor(habitat, levels = c('perm', 'seas', 'rice', 'corn', 
                                              'other', 'whep_vardd', 'whep_fall', 
                                              'br_spring', 'br_fall')))

# add missing values for br and whep equivalent to rice:
energydens <- energydens %>%
  complete(habitat, fill = list(value = energydens$value[energydens$habitat == 'rice'], 
                                lcl = energydens$lcl[energydens$habitat == 'rice'], 
                                ucl = energydens$ucl[energydens$habitat == 'rice']))

# HABITAT AVAILABLE AND ACCESSIBLE--------------

load(here::here(habitat_change))
str(change_all, max.level = 2)



# BASIC DETERMINISTIC MODEL-------------------

## with population objectives:
obj_det <- map(change_all, 
               ~ run_bioenergmod_loop(habitat.available = .x$openwater,
                                      habitat.accessible = .x$accessible,
                                      habitat.added = .x$added,
                                      habitat.returned = .x$returned,
                                      prop.accessible = .x$prop.accessible,
                                      energyneed = needs$DER.obj,
                                      energydens = energydens))

## with baseline/"current" population size:
obs_det <- map(change_all, 
               ~ run_bioenergmod_loop(habitat.available = .x$openwater,
                                      habitat.accessible = .x$accessible,
                                      habitat.added = .x$added,
                                      habitat.returned = .x$returned,
                                      prop.accessible = .x$prop.accessible,
                                      energyneed = needs$DER.obs,
                                      energydens = energydens))


## repeat without incentive acres:
## - with population objectives:
obj_det2 <- map(change_all, 
                ~ run_bioenergmod_loop(habitat.available = .x$openwater %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       habitat.accessible = .x$accessible %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       habitat.added = .x$added %>%
                                         select(-br_fall, -br_spring,
                                                -whep_vardd, -whep_fall),
                                       habitat.returned = .x$returned %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       prop.accessible = .x$prop.accessible %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       energyneed = needs$DER.obj,
                                       energydens = energydens))

## - with baseline/"current" population size:
obs_det2 <- map(change_all, 
                ~ run_bioenergmod_loop(habitat.available = .x$openwater %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       habitat.accessible = .x$accessible %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       habitat.added = .x$added %>%
                                         select(-br_fall, -br_spring,
                                                -whep_vardd, -whep_fall),
                                       habitat.returned = .x$returned %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       prop.accessible = .x$prop.accessible %>%
                                         select(-br_fall, -br_spring, 
                                                -whep_vardd, -whep_fall),
                                       energyneed = needs$DER.obs,
                                       energydens = energydens))

results <- list(obj_det = obj_det, obj_det2 = obj_det2, 
                obs_det = obs_det, obs_det2 = obs_det2)

save(results, file = here::here(bioenerg_results))


# RESULTS EXTRACTION-----------
key <- tibble(scenario = c('obj_det', 'obj_det2', 'obs_det', 'obs_det2'),
              incentives = c('all', 'none', 'all', 'none'),
              population = c('objectives', 'objectives', 'baseline', 'baseline'))

energysum <- map_dfr(results, 
                     ~map_dfr(.x, ~.x[['energy']], .id = 'group'), 
                     .id = 'scenario') %>%
  left_join(key, by = 'scenario')
write_csv(energysum, here::here(results_energy))

accessible <- map_dfr(results, 
                      ~map_dfr(.x, ~.x[['energy.accessible']], .id = 'group'), 
                      .id = 'scenario') %>%
  left_join(key, by = 'scenario')
write_csv(accessible, here::here(results_energy_accessible))

consumed <- map_dfr(results, 
                    ~map_dfr(.x, ~.x[['energy.consumed']], .id = 'group'), 
                    .id = 'scenario') %>%
  left_join(key, by = 'scenario')
write_csv(consumed, here::here(results_energy_consumed))

lost <- map_dfr(results, 
                ~map_dfr(.x, ~.x[['energy.lost']], .id = 'group'), 
                .id = 'scenario') %>%
  left_join(key, by = 'scenario')
write_csv(lost, here::here(results_energy_lost))

