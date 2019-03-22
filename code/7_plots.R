# README----------
# Produce publication and presentation-ready plots

# PACKAGES
library(tidyverse)

# INPUTS
floodcurves <- 'output/open_water_annual.csv'
habitat_change <- 'output/habitat_change.RData'
habitat_stats <- 'output/habitat_peak_stats.csv'

# OUTPUTS
plot_floodcurves <- 'figs/flood_curves.png'
plot_habitat_open <- 'figs/habitat_open.png'
plot_habitat_accessible <- 'figs/habitat_accessible.png'

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
# order variables in stack from top to bottom (and match colors)
levels = c('br', 'whep_fall', 'whep_vardd', 'rice', 'corn', 'other', 'seas', 'perm')
pal = c(pointblue.palette[c(10, 11, 9, 3)], 'yellow', pointblue.palette[c(2, 1, 4)])
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

