# README----------
# Produce publication and presentation-ready plots

# PACKAGES
library(tidyverse)

# INPUTS
base <- 'data/landcover_totals_cvjv.csv'
floodcurves <- 'output/open_water_annual.csv'
orig_curves <- 'data/cvjv_orig/flooding_curves.csv' #original CVJV flood curves
habitat_daily <- 'output/habitat_daily_stats.csv'
habitat_stats <- 'output/habitat_peak_stats.csv'
results_energy <- 'output/bioenergetics_results_energy.csv'
results_energy_accessible <- 'output/bioenergetics_results_energy_accessible.csv'
results_energy_consumed <- 'output/bioenergetics_results_energy_consumed.csv'
results_energy_lost <- 'output/bioenergetics_results_energy_lost.csv'

# OUTPUTS
plot_baseline <- 'figs/baseline_habitat.png'
plot_floodcurves <- 'figs/flood_curves.png'
plot_habitat_open <- 'figs/habitat_open.png'
plot_habitat_accessible <- 'figs/habitat_accessible.png'
plot_energy_accessible <- 'figs/energy_accessible_by_year.png'
plot_energy_consumed <- 'figs/energy_consumed_by_year.png'
plot_energy_shortfall <- 'figs/energy_shortfall_by_year.png'
plot_energy_shortfall_baseline <- 'figs/energy_shortfall_by_year_baseline.png'
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

pal = c(pointblue.palette[c(8, 10, 11, 9, 3)], 'yellow', pointblue.palette[c(2, 1, 4)])

# order variables in stack from top to bottom (and match colors)
levels = c('br_fall', 'br_spring', 'whep_fall', 'whep_vardd', 
           'rice', 'corn', 'other', 'seas', 'perm')

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

# BASELINE HABITAT-----------
scale = 1000
cvjv = data.frame(habitat = c('corn', 'other', 'rice'),
                  area = c(105613, 830293, 219082))

read_csv(here::here(base), col_types = cols()) %>% 
  filter(!habitat %in% c('seas', 'perm') & year < 2017) %>%
  mutate(habitat = factor(habitat, levels = c('wetlands', 'rice', 'corn', 'other'))) %>%
  ggplot(aes(year, area/1000, color = habitat)) + geom_line(size = 1) + geom_point(size = 2) + 
  scale_color_manual(values = pointblue.palette[c(4, 3, 11, 2)]) +
  ylab('area planted (ha, thousands)') + xlab(NULL) +
  # add average lines from CVJV
  geom_hline(data = cvjv, aes(yintercept = area/scale), linetype = 'dashed')
ggsave(here::here(plot_baseline), height = 4, width = 6.5, units = 'in', dpi = 350)

# FLOODING CURVES-------
# replace curve for "other" crops with original CVJV flood curve
replace <- expand.grid(group = c('2013-14', '2014-15', '2015-16', '2016-17'), 
                       yday = c(1:319)) %>% 
  left_join(read_csv(here::here(orig_curves), col_types = cols()) %>% 
              filter(habitat == 'other'), 
            by = 'yday') %>%
  arrange(group, yday)

floodpred <- read_csv(here::here(floodcurves), col_types = cols()) %>%
  filter(habitat != 'other') %>%
  bind_rows(replace) %>%
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

habitat_avail <- read_csv(here::here(habitat_daily), col_types = cols())

peaks <- read_csv(here::here(habitat_stats), col_types = cols())

# open water by year:

habitat_avail %>%
  filter(watertype == 'open') %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot() + facet_wrap(~group, nrow = 4) +
  geom_area(aes(time, value/scale, fill = habitat)) +
  scale_fill_manual(values = pal) +
  timeaxis + timetheme + ylab('Open water (ha, thousands)') +
  # add line for peak value in each year
  geom_vline(data = peaks %>% filter(watertype == 'open'), 
             aes(xintercept = time, linetype = incentives))
ggsave(here::here(plot_habitat_open), width = 6.5, height = 7.5, units = 'in')
  
# accessible water by year:

habitat_avail %>%
  filter(watertype == 'accessible') %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels)) %>%
  ggplot() + facet_wrap(~group, nrow = 4) +
  geom_area(aes(time, value/scale, fill = habitat)) +
  scale_fill_manual(values = pal) +
  timeaxis + timetheme + ylab('Accessible open water (ha, thousands)') +
  # add line for peak value in each year
  geom_vline(data = peaks %>% filter(watertype == 'accessible'), 
             aes(xintercept = time, linetype = incentives))
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
  group_by(scenario, group, time, incentives, population) %>%
  summarize(value = sum(value, na.rm = T)) %>% # total energy by day of year
  ungroup() %>%
  filter(time <= 300) %>% #avoid late peaks in rice
  group_by(scenario, group, incentives, population) %>%
  summarize(time = time[value == max(value, na.rm = T)],
            value = max(value, na.rm = T))

accessible %>% filter(scenario == 'obj_det') %>%
  ggplot(aes(time, value/scale)) + 
  facet_wrap(~group, nrow = 4) +
  geom_area(aes(fill = habitat)) + 
  scale_fill_manual(values = pal) +
  geom_line(aes(y = DER/scale)) +
  timeaxis + timetheme +
  ylab('Energy accessible (kJ, billions)') +
  geom_vline(data = peak_accessible %>% filter(population == 'objectives'), 
             aes(xintercept = time, linetype = incentives))
ggsave(here::here(plot_energy_accessible), height = 8, width = 6.5, units = 'in', dpi = 350)

accessible %>% filter(scenario == 'obs_det2') %>%
  ggplot(aes(time, value/scale)) + 
  facet_wrap(~group, nrow = 4) +
  geom_area(aes(fill = habitat)) + 
  scale_fill_manual(values = pal) +
  geom_line(aes(y = DER/scale)) +
  timeaxis + timetheme +
  ylab('Energy accessible (kJ, billions)') +
  geom_vline(data = peak_accessible %>% filter(population == 'baseline'), 
             aes(xintercept = time, linetype = incentives))

# change in peak accessible by scenario & incentive programs:
peak_accessible %>% 
  ungroup() %>%
  select(-scenario, -time) %>% 
  spread(key = incentives, value = value) %>%
  mutate(prop_all = (all - none)/none,
         prop_fall = (`fall only` - none)/none) %>%
  arrange(population, group) 

# change in timing of peak accessible by scenario & incentive programs:
#  -- fall programs do not affect this peak; vardd and br_spring advance peak 
#     in 2013-14 and 2014-15 but delay peak in 2015-16 and 2016-17 (slightly)
peak_accessible %>% 
  ungroup() %>%
  select(-scenario, -value) %>% 
  spread(key = incentives, value = time) %>%
  arrange(population, group) 

# ENERGY CONSUMED----------------
scale = 1000000

consumed <- read_csv(here::here(results_energy_consumed), col_types = cols()) %>%
  gather(corn:whep_vardd, key = 'habitat', value = 'value') %>%
  left_join(read_csv(here::here(results_energy), col_types = cols()) %>% 
              select(scenario:DER),
            by = c('scenario', 'group', 'time')) %>%
  mutate(habitat = factor(habitat, levels = levels))

total_consumed <- consumed %>% 
  group_by(scenario, group, incentives, population, habitat) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(total = sum(value),
         prop = value / total)

# ggplot(consumed %>% filter(scenario == 'obj_det'), aes(time, value/scale)) + 
#   facet_wrap(~group, nrow = 4) +
#   geom_area(aes(fill = habitat)) + 
#   scale_fill_manual(values = pal) +
#   geom_line(aes(y = DER/scale)) +
#   timeaxis + timetheme +
#   ylab('Energy consumed (kJ, millions)')

ggplot(total_consumed %>% filter(scenario == 'obj_det'), 
       aes(x = group, y = prop, fill = habitat)) + 
  # facet_wrap(~group, nrow = 4) +
  geom_col() +
  scale_fill_manual(values = pal) +
  labs(x = NULL, y = NULL) + 
  timetheme + theme(panel.grid = element_blank()) + 
  scale_y_continuous(expand = c(0,0))
ggsave(here::here(plot_energy_consumed), height = 4, width = 6.5, units = 'in', dpi = 350)

# total proportion of energy consumed in incentivized acres
total_consumed %>%
  ungroup() %>%
  select(-scenario) %>%
  filter(habitat %in% c('br_fall', 'br_spring', 'whep_fall', 'whep_vardd')) %>% 
  group_by(population, group, incentives) %>% 
  summarize(value = sum(value),
            total = max(total),
            prop = value / total) %>%
  arrange(population, incentives, group) %>%
  filter(population == 'objectives')

# impact of incentive programs on energy consumed in wetlands
total_consumed %>% 
  filter(habitat %in% c('seas', 'perm') & population == 'objectives') %>% 
  group_by(scenario, incentives, population, group) %>% 
  summarize(value = sum(value)) %>% 
  ungroup() %>% 
  select(-scenario) %>% 
  spread(key = incentives, value = value) %>% 
  mutate(prop_all = (all - none)/none,
         prop_fall = (`fall only` - none)/none)

# ENERGY SHORTFALLS---------------
scale = 1000000 #millions

energy <- read_csv(here::here(results_energy), col_types = cols()) 

# population objectives:
energy %>%
  filter(incentives %in% c('all', 'none') & population == 'objectives') %>%
  mutate(incentives = recode(incentives, all = 'with', none = 'without'),
         incentives = factor(incentives, levels = c('without', 'with'))) %>%
  ggplot(aes(time, shortfall/scale)) + 
  facet_wrap(~group, nrow = 4) + 
  geom_area(aes(fill = incentives), position = position_identity()) + 
  scale_fill_manual(values = pointblue.palette[c(3,4)]) + 
  # geom_text(data = obj_short, aes(mid, 25, label = round(value/scale2, 0))) +
  labs(x = NULL, y = 'Energy shortfall (kJ, millions)') +
  timeaxis + timetheme +
  geom_line(aes(y = DER/scale))
ggsave(here::here(plot_energy_shortfall), height = 8, width = 6.5, units = 'in', dpi = 350)

# compare baseline population size:
energy %>%
  filter(incentives %in% c('all', 'none') & population == 'baseline') %>%
  mutate(incentives = recode(incentives, all = 'with', none = 'without'),
         incentives = factor(incentives, levels = c('without', 'with'))) %>%
  ggplot(aes(time, shortfall/scale)) + 
  facet_wrap(~group, nrow = 4) + 
  geom_area(aes(fill = incentives), position = position_identity()) + 
  scale_fill_manual(values = pointblue.palette[c(3,4)]) + 
  # geom_text(data = obj_short, aes(mid, 25, label = round(value/scale2, 0))) +
  labs(x = NULL, y = 'Energy shortfall (kJ, millions)') +
  timeaxis + timetheme +
  geom_line(aes(y = DER/scale))
ggsave(here::here(plot_energy_shortfall_baseline), height = 8, width = 6.5, units = 'in', dpi = 350)

# % reduction in seasonal shortfalls contributed by incentive programs
reduction <- energy %>% 
  mutate(season = case_when(time <= 184 ~ 'fall',
                            time > 184 ~ 'spring')) %>%
  group_by(scenario, group, incentives, population, season) %>%
  summarize(value = sum(shortfall)) %>%
  ungroup() %>%
  select(-scenario) %>%
  spread(key = incentives, value = value) %>%
  mutate(diff_all = (none - all)/none * 100,
         diff_all = case_when(is.nan(diff_all) ~ 0,
                          TRUE ~ diff_all),
         diff_fall = (none - `fall only`)/none * 100,
         diff_fall = case_when(is.nan(diff_fall) ~ 0,
                               TRUE ~ diff_fall),
         diff_spring = diff_all - diff_fall) %>%
  arrange(population, season, group)


reduction %>%
  select(group, population, season, diff_all) %>%
  ggplot(aes(group, diff_all, fill = season)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~population) +
  # select(group, population, season, diff_fall, diff_spring) %>%
  # gather(diff_fall:diff_spring, key = 'incentives', value = 'value') %>% 
  # mutate(incentives = factor(incentives, levels = c('diff_spring', 'diff_fall')),
  #        incentives = recode(incentives, 
  #                            diff_fall = 'fall programs', 
  #                            diff_spring = 'spring programs')) %>%
  # ggplot(aes(group, value, fill = incentives)) +
  # geom_col() +
  # facet_wrap(~population+season, nrow = 2) +
  scale_fill_manual(values = pointblue.palette[c(2,4)]) +
  labs(x = NULL, y = '% reduction in energy shortfalls') + 
  theme_minimal() + theme(strip.text = element_text(hjust = 0)) +
  ylim(0, 100)
ggsave(here::here(plot_energy_effect), height = 4, width = 6.5, units = 'in', dpi = 350)
