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

# ENERGY DENSITY------------

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
                                         select(-br, -whep_vardd, -whep_fall),
                                       habitat.accessible = .x$accessible %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       habitat.added = .x$added %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       habitat.returned = .x$returned %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       prop.accessible = .x$prop.accessible %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       energyneed = needs$DER.obj,
                                       energydens = energydens))

## - with baseline/"current" population size:
obs_det2 <- map(change_all, 
                ~ run_bioenergmod_loop(habitat.available = .x$openwater %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       habitat.accessible = .x$accessible %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       habitat.added = .x$added %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       habitat.returned = .x$returned %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       prop.accessible = .x$prop.accessible %>%
                                         select(-br, -whep_vardd, -whep_fall),
                                       energyneed = needs$DER.obs,
                                       energydens = energydens))

# PLOT ACCESSIBLE ENERGY----------------

## control the order of stacked area plots:
levels <- c('perm', 'seas', 'other', 'corn', 'rice', 'whep_vardd', 'whep_fall', 'br')

## compile values across years:

obj_accessible <- map_dfr(obj_det, ~.x[['energy.accessible']], 
                          .id = 'group') %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = rev(levels))) %>%
  left_join(needs %>% select(yday, DER.obj), by = c('time' = 'yday'))

obs_accessible <- map_dfr(obs_det, ~.x[['energy.accessible']], 
                          .id = 'group') %>%
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

obj_energy <- map_dfr(obj_det, ~.x[['energy']], .id = 'group') %>%
  full_join(map_dfr(obj_det2, ~.x[['energy']] %>% select(time, shortfall2 = shortfall), 
                    .id = 'group'),
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

obs_energy <- map_dfr(obs_det, ~.x[['energy']], .id = 'group') %>%
  full_join(map_dfr(obs_det2, ~.x[['energy']] %>% select(time, shortfall2 = shortfall), 
                    .id = 'group'),
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

