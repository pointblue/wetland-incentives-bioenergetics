# README----------
# Produce publication and presentation-ready plots

# PACKAGES
library(tidyverse)

# INPUTS
floodcurves <- 'output/open_water_annual.csv'
habitat_change <- 'output/habitat_change.RData'
habitat_stats <- 'output/habitat_peak_stats.csv'
results_energy <- 'output/bioenergetics_results_energy.csv'
results_energy_accessible <- 'output/bioenergetics_results_energy_accessible.csv'
results_energy_consumed <- 'output/bioenergetics_results_energy_consumed.csv'
results_energy_lost <- 'output/bioenergetics_results_energy_lost.csv'

# OUTPUTS
plot_floodcurves <- 'figs/flood_curves.png'
plot_habitat_open <- 'figs/habitat_open.png'
plot_habitat_accessible <- 'figs/habitat_accessible.png'
plot_energy_accessible <- 'figs/energy_accessible_by_year.png'
plot_energy_shortfall <- 'figs/energy_shortfall_by_year.png'
plot_energy_effect <- 'figs/energy_effect_by_year.png'

# ALL PURPOSE PLOT STANDARDS------------
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

# order variables in stack from top to bottom (and match colors)
levels = c('br', 'whep_fall', 'whep_vardd', 'rice', 'corn', 'other', 'seas', 'perm')

timeaxis = list(scale_x_continuous(breaks = c(1, 15.5, 32, 47.5, 63, 78, 
                                              93, 108.5, 124, 139, 154, 169.5, 
                                              185, 200.5, 216, 230, 244, 259.5, 
                                              275, 290, 305, 320.5, 336), 
                                   labels = c('', 'Jul', '', 'Aug', '', 'Sep', 
                                              '', 'Oct', '', 'Nov', '', 'Dec', 
                                              '', 'Jan', '', 'Feb', '', 'Mar', 
                                              '', 'Apr', '', 'May', ''),
                                   limits = c(0, 336),
                                   expand = c(0, 0)),
                xlab(NULL))

timetheme = theme_classic() + 
  theme(panel.grid.major.x = element_line(color = c(rep(c('gray90', NA), 
                                                        length(timeaxis[[1]]$breaks - 1)/2), 
                                                    'gray90')),
        panel.grid.major.y = element_line(color = 'gray90'),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank(),
        axis.ticks.x = element_line(color = c(rep(c('black', NA), 
                                                  length(timeaxis[[1]]$breaks - 1)/2), 
                                              'black')))

# FLOODING CURVES-------
floodpred <- read_csv(here::here(floodcurves), col_types = cols()) %>%
  mutate(habitat = factor(habitat, 
                          levels = c('wetlands', 'rice', 'corn', 'other')),
         habitat = recode(habitat, 
                          other = 'other crops'),
         group = factor(group, 
                        levels = c('2016-17', '2015-16', '2014-15', '2013-14'))) %>%
  rename(year = group)

ggplot(floodpred, aes(yday, fit)) + 
  facet_wrap(~habitat, nrow = 4) + 
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = year), alpha = 0.6) +
  geom_line(aes(color = year)) +
  scale_fill_viridis_d() + scale_color_viridis_d() +
  ylim(0, 1) + ylab('Proportion open water') + timeaxis + timetheme 
ggsave(here::here(plot_floodcurves), width = 6.5, height = 7.5, units = 'in')


# HABITAT AVAILABILITY------------------
scale = 1000

load(here::here(habitat_change))
habitat_open <- map_dfr(change_all, ~.x[['openwater']], .id = 'group')
habitat_accessible <- map_dfr(change_all, ~.x[['accessible']], .id = 'group')

peaks <- read_csv(here::here(habitat_stats), col_types = cols()) %>%
  mutate(habitat = recode(habitat, 
                          all = 'with incentive programs', 
                          free = 'without incentive programs'))


# open water by year:

habitat_open %>%
  gather(-group, -time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot() + facet_wrap(~group, nrow = 4) +
  geom_area(aes(time, value/scale, fill = habitat)) +
  scale_fill_manual(values = pal) +
  timeaxis + timetheme + ylab('Open water (ha, thousands)') +
  # add line for peak value in each year
  geom_vline(data = peaks %>% filter(type == 'open'), 
             aes(xintercept = time, linetype = habitat))
ggsave(here::here(plot_habitat_open), width = 6.5, height = 7.5, units = 'in')

# accessible water by year:

habitat_accessible %>%
  gather(-group, -time, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot() + facet_wrap(~group, nrow = 4) +
  geom_area(aes(time, value/scale, fill = habitat)) +
  scale_fill_manual(values = pal) +
  timeaxis + timetheme + ylab('Accessible open water (ha, thousands)') +
  # add line for peak value in each year
  geom_vline(data = peaks %>% filter(type == 'accessible'), 
             aes(xintercept = time, linetype = habitat))
ggsave(here::here(plot_habitat_accessible), height = 8, width = 6.5, units = 'in', dpi = 350)

# ENERGY ACCESSIBLE-------------------
scale = 1000000000 #billion

accessible <- read_csv(here::here(results_energy_accessible), col_types = cols()) %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  left_join(read_csv(here::here(results_energy), col_types = cols()) %>% 
              select(scenario:DER),
            by = c('scenario', 'group', 'time')) %>%
  mutate(habitat = factor(habitat, levels = levels))

peak_accessible <- accessible %>% 
  group_by(scenario, group, time) %>%
  summarize(value = sum(value, na.rm = T)) %>% # total energy by day of year
  ungroup() %>%
  group_by(scenario, group) %>%
  summarize(time = time[value == max(value, na.rm = T)],
            value = max(value, na.rm = T)) %>%
  mutate(incentives = case_when(scenario %in% c('obj_det', 'obs_det') ~ 'with incentive programs',
                                TRUE ~ 'without incentive programs'))


ggplot(accessible %>% filter(scenario == 'obj_det'), aes(time, value/scale)) + 
  facet_wrap(~group, nrow = 4) +
  geom_area(aes(fill = habitat)) + 
  scale_fill_manual(values = pal) +
  geom_line(aes(y = DER/scale)) +
  timeaxis + timetheme +
  ylab('Energy accessible (kJ, billions)') +
  geom_vline(data = peak_accessible %>% filter(scenario %in% c('obj_det', 'obj_det2')), 
             aes(xintercept = time, linetype = incentives))
ggsave(here::here(plot_energy_accessible), height = 8, width = 6.5, units = 'in', dpi = 350)


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
