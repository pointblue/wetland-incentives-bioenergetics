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

plot_energy_accessible <- 'figs/energy_accessible_by_year.png'
plot_energy_shortfall <- 'figs/energy_shortfall_by_year.png'
plot_energy_effect <- 'figs/energy_effect_by_year.png'



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

energydens <- read_csv(here::here(energy_density), col_types = cols()) %>%
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

results <- list(obj_det = obj_det, obj_det2 = obj_det2, obs_det = obs_det, obs_det2 = obs_det2)

save(results, file = here::here(bioenerg_results))


# RESULTS EXTRACTION-----------
energysum <- map_dfr(results, ~map_dfr(.x, ~.x[['energy']], .id = 'group'), .id = 'scenario')
write_csv(energysum, here::here(results_energy))

accessible <- map_dfr(results, ~map_dfr(.x, ~.x[['energy.accessible']], .id = 'group'), .id = 'scenario')
write_csv(accessible, here::here(results_energy_accessible))

consumed <- map_dfr(results, ~map_dfr(.x, ~.x[['energy.consumed']], .id = 'group'), .id = 'scenario')
write_csv(consumed, here::here(results_energy_consumed))

lost <- map_dfr(results, ~map_dfr(.x, ~.x[['energy.lost']], .id = 'group'), .id = 'scenario')
write_csv(lost, here::here(results_energy_lost))


# ACCESSIBLE ENERGY----------------

## control the order of stacked area plots:
levels <- c('perm', 'seas', 'other', 'corn', 'rice', 'whep_vardd', 'whep_fall', 'br')

## compile values across years, compare to energy needs
obj_accessible <- map_dfr(obj_det, ~.x[['energy.accessible']], 
                          .id = 'group') %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  # mutate(habitat = factor(habitat, levels = rev(levels))) %>%
  left_join(needs %>% select(yday, DER.obj), by = c('time' = 'yday'))

obs_accessible <- map_dfr(obs_det, ~.x[['energy.accessible']], 
                          .id = 'group') %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = rev(levels))) %>%
  left_join(needs %>% select(yday, DER.obs), by = c('time' = 'yday'))

## peak accessible by year
peak_obj_accessible <- obj_accessible %>% 
  group_by(group, time) %>%
  summarize(energy = sum(value)) %>% # total energy by day of year
  group_by(group) %>%
  summarize(time = time[energy == max(energy)],
            energy = max(energy))

peak_obs_accessible <- obs_accessible %>% 
  group_by(group, time) %>%
  summarize(energy = sum(value)) %>% # total energy by day of year
  group_by(group) %>%
  summarize(time = time[energy == max(energy)],
            energy = max(energy))

## plot
pointblue.palette <-
  c('#4495d1',
    '#74b743',
    '#f7941d',
    '#005baa',
    '#bfd730',
    '#a7a9ac',
    '#666666',
    '#456d28', #add a few more complementary colors
    '#b74374', 
    '#5e2a84',
    '#d2c921')

pal = c(pointblue.palette[c(10, 11, 9, 3)], 'yellow', pointblue.palette[c(2, 1, 4)])

ymax = 8
der = needs$DER.obj
scale = 1000000000 #billion
ylab = 'Accessible energy: kJ (billions)'
theme = theme_classic() + 
  theme(panel.grid = element_line(color = NA),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank())
scalex = scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 
                                       185, 216, 244, 275, 305), 
                            labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 
                                       'Jan', 'Feb', 'Mar', 'Apr', 'May'))

a <- ggplot(obj_accessible, aes(time, value/scale)) + facet_wrap(~group, nrow = 4) +
  geom_area(aes(fill = habitat)) + 
  geom_line(aes(y = DER.obj/scale)) +
  geom_vline(data = peak_obj_accessible, aes(xintercept = time)) +
  labs(y = ylab, x = NULL) + ylim(0, ymax) +
  scale_fill_manual(values = pal) +
  guides(fill = guide_legend(ncol = 3)) +
  scalex + theme + theme(legend.position = 'none') + 
  ggtitle('Population objectives')

b <- ggplot(obs_accessible, aes(time, value/scale)) + facet_wrap(~group, nrow = 4) +
  geom_area(aes(fill = habitat)) + 
  geom_line(aes(y = DER.obs/scale)) +
  geom_vline(data = peak_obs_accessible, aes(xintercept = time)) +
  labs(y = NULL, x = NULL) + 
  scale_y_continuous(limits = c(0, ymax), labels = NULL) +
  scale_fill_manual(values = pal) +
  scalex + theme +
  ggtitle('Baseline population')

cowplot::plot_grid(a, b, ncol = 2, rel_widths = c(0.45, 0.55))
ggsave(here::here(plot_energy_accessible))


# ENERGY SHORTFALLS---------------
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

c <- ggplot(obj_energy, aes(time, value/scale2, fill = scenario)) + 
  geom_area(position = position_identity()) + 
  facet_wrap(~group, nrow = 4) + 
  # geom_text(data = obj_short, aes(mid, 25, label = round(value/scale2, 0))) +
  labs(x = NULL, y = ylab2) +
  ylim(0, ymax2) + scalex + theme + theme(legend.position = 'none') +
  scale_fill_manual(values = pointblue.palette[c(3,4)]) + 
  ggtitle('Population objectives')

d <- ggplot(obs_energy, aes(time, value/scale2, fill = scenario)) + 
  geom_area(position = position_identity()) + 
  facet_wrap(~group, nrow = 4) + 
  # geom_text(data = obj_short, aes(mid, 25, label = round(value/scale2, 0))) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(limits = c(0, ymax2), labels = NULL) + scalex + theme +
  scale_fill_manual(values = pointblue.palette[c(3,4)]) + 
  ggtitle('Baseline population')

cowplot::plot_grid(c, d, ncol = 2, rel_widths = c(0.40, 0.60))
ggsave(here::here(plot_energy_shortfall))


# alternative bar graph:
obj_short <- obj_energy %>% 
  mutate(season = case_when(time <= 184 ~ 'fall',
                            time > 184 ~ 'spring')) %>%
  group_by(group, season, scenario) %>%
  summarize(mid = mean(time[value > 0]),
            value = sum(value))

obs_short <- obs_energy %>% 
  mutate(season = case_when(time <= 184 ~ 'fall',
                            time > 184 ~ 'spring')) %>%
  group_by(group, season, scenario) %>%
  summarize(mid = mean(time[value > 0]),
            value = sum(value))

e <- obj_short %>%
  select(-mid) %>%
  spread(key = scenario, value = value) %>%
  mutate(diff = (`without incentive programs` - `with incentive programs`)/`without incentive programs`) %>%
  ggplot(aes(group, diff)) +
  geom_col(position = position_dodge(), fill = 'gray50') + facet_wrap(~season, nrow = 2) +
  labs(x = NULL, y = '% reduction from incentive programs', title = 'Population objectives') + 
  theme + ylim(0,1)

f <- obs_short %>%
  select(-mid) %>%
  spread(key = scenario, value = value) %>%
  mutate(diff = (`without incentive programs` - `with incentive programs`)/`without incentive programs`,
         diff = case_when(is.nan(diff) ~ NA_real_,
                          TRUE ~ diff)) %>%
  ggplot(aes(group, diff)) +
  geom_col(position = position_dodge(), fill = 'gray50') + facet_wrap(~season, nrow = 2) +
  labs(x = NULL, y = NULL, title = 'Baseline population') + 
  theme + 
  scale_y_continuous(limits = c(0,1), labels = NULL)

cowplot::plot_grid(e, f, ncol = 2, rel_widths = c(0.52, 0.48))
ggsave(here::here(plot_energy_effect))
