# README---------------
# Run bioenergetics models
#
# PACKAGES
library(tidyverse)
library(bioenergmod) # devtools::install_github("kdybala/bioenergmod")

# INPUTS
objectives <- 'data/cvjv_orig/daily_energy_requirement.csv'
energy_density <- 'data/cvjv_orig/energy_content.csv'
habitat_available <- 'output/habitat_available.csv'
habitat_accessible <- 'output/habitat_accessible.csv'
habitat_added <- 'output/habitat_added.csv'
habitat_returned <- 'output/habitat_returned.csv'
habitat_prop.accessible <- 'output/habitat_prop.accessible.csv'

# OUTPUTS



# ENERGY NEEDS------------

needs <- read_csv(here::here(objectives))

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

# ENERGY AVAILABLE------------

energydens <- read_csv(here::here(energy_density)) %>%
  filter(habitat != 'corn') %>%
  mutate(habitat = recode(habitat, corn_north = 'corn'),
         habitat = factor(habitat, levels = c('perm', 'seas', 'rice', 'corn', 
                                              'other', 'whep_vardd', 
                                              'whep_fall', 'br')))

# add missing values for br and whep equivalent to rice:
energydens <- energydens %>%
  complete(habitat, fill = list(value = energydens$value[energydens$habitat == 'rice'], 
                                lcl = energydens$lcl[energydens$habitat == 'rice'], 
                                ucl = energydens$ucl[energydens$habitat == 'rice']))

available <- read_csv(here::here(habitat_available)) %>%
  split(.$group) %>%
  map(~ (.x %>% select(-group)))

accessible <- read_csv(here::here(habitat_accessible)) %>%
  split(.$group) %>%
  map(~ (.x %>% select(-group)))

added <- read_csv(here::here(habitat_added)) %>% 
  mutate(br = case_when(is.na(br) ~ 0,
                        TRUE ~ br)) %>%
  split(.$group) %>%
  map(~ (.x %>% select(-group)))

returned <- read_csv(here::here(habitat_returned)) %>% 
  mutate(br = case_when(is.na(br) ~ 0,
                        TRUE ~ br)) %>%
  split(.$group) %>%
  map(~ (.x %>% select(-group)))

prop.accessible <- read_csv(here::here(habitat_prop.accessible)) %>%
  split(.$group) %>%
  map(~ (.x %>% select(-group)))


# BASIC DETERMINISTIC MODEL-------------------

## with population objectives:
obj_det <- pmap(list(available, accessible, added, returned, prop.accessible),
                ~ run_bioenergmod_loop(energyneed = needs$DER.obj, 
                                       energydens = energydens,
                                       habitat.available = ..1,
                                       habitat.accessible = ..2,
                                       habitat.added = ..3,
                                       habitat.returned = ..4,
                                       prop.accessible = ..5))

## with baseline/"current" population size:
obs_det <- pmap(list(available, accessible, added, returned, prop.accessible),
                ~ run_bioenergmod_loop(energyneed = needs$DER.obs, 
                                       energydens = energydens,
                                       habitat.available = ..1,
                                       habitat.accessible = ..2,
                                       habitat.added = ..3,
                                       habitat.returned = ..4,
                                       prop.accessible = ..5))


## repeat without incentive acres:
## - with population objectives:
obj_det2 <- pmap(list(available %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      accessible %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      added %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      returned %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      prop.accessible %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall)))),
                ~ run_bioenergmod_loop(energyneed = needs$DER.obj, 
                                       energydens = energydens,
                                       habitat.available = ..1,
                                       habitat.accessible = ..2,
                                       habitat.added = ..3,
                                       habitat.returned = ..4,
                                       prop.accessible = ..5))

## with baseline/"current" population size:
obs_det2 <- pmap(list(available %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      accessible %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      added %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      returned %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall))), 
                      prop.accessible %>% map(~ (.x %>% select(-br, -whep_vardd, -whep_fall)))),
                ~ run_bioenergmod_loop(energyneed = needs$DER.obs, 
                                       energydens = energydens,
                                       habitat.available = ..1,
                                       habitat.accessible = ..2,
                                       habitat.added = ..3,
                                       habitat.returned = ..4,
                                       prop.accessible = ..5))

# PLOT ACCESSIBLE ENERGY----------------

## control the order of stacked area plots:
levels <- c('perm', 'seas', 'other', 'corn', 'rice', 'whep_vardd', 'whep_fall', 'br')

## compile values across years:
obj_accessible <- map_dfr(.x = names(obj_det),
                          .f = function(x) {
                            res <- obj_det[[x]][['energy.accessible']] %>%
                              mutate(group = x)}) %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = rev(levels))) %>%
  left_join(needs %>% select(yday, DER.obj), by = c('time' = 'yday'))

obs_accessible <- map_dfr(.x = names(obs_det),
                          .f = function(x) {
                            res <- obs_det[[x]][['energy.accessible']] %>%
                              mutate(group = x)}) %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = rev(levels))) %>%
  left_join(needs %>% select(yday, DER.obs), by = c('time' = 'yday'))

pal = scales::viridis_pal()(8)
ymax = 8
der = needs$DER.obj
scale = 1000000000 #billion
ylab = 'Accessible energy: kJ (billions)'
theme = theme(panel.grid = element_line(color = NA))
scalex = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 
                                       185, 216, 244, 275, 305), 
                            labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 
                                       'Jan', 'Feb', 'Mar', 'Apr', 'May'))

ggplot(obj_accessible, aes(time, value/scale)) + facet_wrap(~group) +
  geom_area(aes(fill = habitat)) + 
  geom_line(aes(y = DER.obj/scale)) +
  labs(y = ylab, x = NULL) + ylim(0, ymax) +
  scale_fill_manual(values = pal) +
  scalex + theme_bw() + theme

ggplot(obs_accessible, aes(time, value/scale)) + facet_wrap(~group) +
  geom_area(aes(fill = habitat)) + 
  geom_line(aes(y = DER.obs/scale)) +
  labs(y = ylab, x = NULL) + ylim(0, ymax) +
  scale_fill_manual(values = pal) +
  scalex + theme_bw() + theme


# ENERGY SHORTFALLS: Objectives---------------
scale2 = 1000000
ylab2 = 'Energy shortfall: kJ (millions)'
ymax2 = 250

obj_energy <- map_dfr(.x = names(obj_det),
                      .f = function(x) {
                        res <- obj_det[[x]][['energy']] %>%
                          mutate(group = x)}) %>%
  full_join(map_dfr(.x = names(obj_det2),
                    .f = function(x) {
                      res <- obj_det2[[x]][['energy']] %>%
                        mutate(group = x) %>%
                        select(time, group, shortfall2 = shortfall)}),
            by = c('time', 'group')) %>%
  select(time, group, shortfall, shortfall2) %>%
  gather(shortfall:shortfall2, key = 'scenario', value = 'value') %>%
  mutate(scenario = recode(scenario,
                           shortfall = 'with incentive programs',
                           shortfall2 = 'without incentive programs'),
         scenario = factor(scenario, levels = c('without incentive programs', 'with incentive programs')))

obj_short <- obj_energy %>% 
  mutate(season = case_when(time <= 184 ~ 'fall',
                            time > 184 ~ 'spring')) %>%
  group_by(group, season, scenario) %>%
  summarize(mid = mean(time[value > 0]),
            value = sum(value))

ggplot(obj_energy, aes(time, value/scale2, fill = scenario)) + 
  geom_area(position = position_identity()) + 
  facet_wrap(~group, nrow = 4) + 
  # geom_text(data = obj_short, aes(mid, 25, label = round(value/scale2, 0))) +
  labs(x = '', y = ylab2) +
  ylim(0, ymax2) + scalex + theme_bw() + theme +
  scale_fill_viridis_d(begin = 0.1, end = 0.85)

ggplot(obj_short, aes(group, value/scale, group = scenario, fill = scenario)) +
  geom_col(position = position_dodge()) + facet_wrap(~season) +
  labs(x = '', y = ylab2) + theme_bw() + theme +
  scale_fill_viridis_d(begin = 0.1, end = 0.85)

# ENERGY SHORTFALLS: Baseline---------------

obs_energy <- map_dfr(.x = names(obs_det),
                      .f = function(x) {
                        res <- obs_det[[x]][['energy']] %>%
                          mutate(group = x)}) %>%
  full_join(map_dfr(.x = names(obs_det2),
                    .f = function(x) {
                      res <- obs_det2[[x]][['energy']] %>%
                        mutate(group = x) %>%
                        select(time, group, shortfall2 = shortfall)}),
            by = c('time', 'group')) %>%
  select(time, group, shortfall, shortfall2) %>%
  gather(shortfall:shortfall2, key = 'scenario', value = 'value') %>%
  mutate(scenario = recode(scenario,
                           shortfall = 'with incentive programs',
                           shortfall2 = 'without incentive programs'),
         scenario = factor(scenario, levels = c('without incentive programs', 'with incentive programs')))

obs_short <- obs_energy %>%  
  mutate(season = case_when(time <= 184 ~ 'fall',
                            time > 184 ~ 'spring')) %>%
  group_by(group, season, scenario) %>%
  summarize(mid = mean(time[value > 0]),
            value = sum(value))

ggplot(obs_energy, aes(time, value/scale2, fill = scenario)) + 
  geom_area(position = position_identity()) + 
  facet_wrap(~group, nrow = 4) + 
  # geom_text(data = obs_short, aes(mid, 25, label = round(value/scale2, 0))) +
  labs(x = '', y = ylab2) +
  ylim(0, ymax2) + scalex + theme_bw() + theme +
  scale_fill_viridis_d(begin = 0.1, end = 0.85)

ggplot(obs_short, aes(group, value/scale, group = scenario, fill = scenario)) +
  geom_col(position = position_dodge()) + facet_wrap(~season) +
  labs(x = '', y = ylab2) + theme_bw() + theme +
  scale_fill_viridis_d(begin = 0.1, end = 0.85)

