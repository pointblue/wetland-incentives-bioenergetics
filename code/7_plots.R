# README----------
# Produce publication and presentation-ready plots
# -- currently all plots are set up for presentations (e.g. large font, in color)

# PACKAGES
library(tidyverse)

# INPUTS
base <- 'data/landcover_totals_cvjv.csv'
floodcurves <- 'output/open_water_annual.csv'
orig_curves <- 'data/cvjv_orig/flooding_curves.csv' #original CVJV flood curves
habitat_daily <- 'output/habitat_daily_stats.csv'
habitat_stats <- 'output/habitat_peak_stats.csv'
results_energy <- 'output/bioenergetics_results_energy.csv'
results_energy_consumed <- 'output/bioenergetics_results_energy_consumed.csv'
results_energy_lost <- 'output/bioenergetics_results_energy_lost.csv'

# OUTPUTS
plot_baseline <- 'figs/baseline_habitat.png'
plot_floodcurves <- 'figs/flood_curves.png'
plot_habitat_open <- 'figs/habitat_open.png'
plot_habitat_accessible <- 'figs/habitat_accessible.png'
plot_habitat_proportions <- 'figs/habitat_proportions.png'
plot_energy_consumed <- 'figs/energy_consumed_by_year.png'
plot_energy_shortfall <- 'figs/energy_shortfall_by_year.png'
plot_energy_shortfall_baseline <- 'figs/energy_shortfall_by_year_baseline.png'
plot_energy_effect <- 'figs/energy_effect_by_year.png'

energy_shortfall_reduction <- 'output/energy_reduction_summary.csv'

# CUSTOM DESIGN------------
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

pal = c(pointblue.palette[c(9, 3)], 'yellow', pointblue.palette[c(2, 4)])

# order variables in stack from top to bottom (and match colors)
levels = c('incentives', 'rice', 'corn', 'other', 'wetlands')

# theme for presentation figs (larger font sizes)
theme_presentation <- theme_classic() + 
  theme(legend.background = element_blank(),
        # legend.position = c(0, 0.5), 
        # legend.justification = c(0, 0.5),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 24, lineheight = 32), 
        legend.key.size = unit(2, 'line'),
        plot.title = element_text(size = 36, face = 'bold', vjust = 1, 
                                  hjust = 0.5),
        axis.text = element_text(size = 24, color = 'black', face = 'plain'), 
        axis.title = element_text(size = 28, face = 'plain', vjust = 0),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20)),
        strip.text = element_text(size = 28, face = 'bold', hjust = 0), 
        strip.background = element_blank(),
        axis.line.x = element_line(color = 'black'),
        axis.line.y = element_line(color = 'black'))


# theme for manuscript figures (smaller font sizes)
theme_manuscript <- theme_classic() + 
  theme(title = element_text(size = 10),
        plot.title = element_text(size = 10, face = 'bold'), 
        axis.text = element_text(size = 9, color = 'black'),
        legend.text = element_text(size = 9, lineheight = 1),
        legend.background = element_blank(),
        legend.key.size = unit(0.5, 'cm'))


# theme for monthly time series (add grid lines and ticks)
timetheme <- theme(
  panel.grid.major.x = element_line(color = c(rep(c('gray90', NA), 
                                                  length(timeaxis[[1]]$breaks - 1)/2), 
                                              'gray90')),
  # panel.grid.major.y = element_line(color = 'gray90'),
  axis.ticks.x = element_line(color = c(rep(c('black', NA), 
                                            length(timeaxis[[1]]$breaks - 1)/2), 
                                        'black')))

# custom axis breaks for monthly time series
timeaxis <- list(scale_x_continuous(breaks = c(1, 15.5, 32, 47.5, 63, 78, 
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


# formatting sizes for manuscript (Global Change Biology)
ms.single = 80 #mm for single column
ms.double = 169 #mm for double column
ms.dpi = 400 #minimum 300 per author guidelines

# formatting sizes for powerpoint template
ppt.width = 10 #inches
ppt.height = 7.5 #inches
ppt.width.wide = 13.33 #inches



# BASELINE HABITAT-----------
scale = 1000
cvjv = data.frame(habitat = c('corn', 'other crops', 'rice', 'wetlands'),
                  area = c(105613, 830293, 219082, 74835))

read_csv(here::here(base), col_types = cols()) %>% 
  filter(!habitat %in% c('seas', 'perm') & year < 2017) %>%
  mutate(habitat = recode(habitat, other = 'other crops'),
         habitat = factor(habitat, levels = c('other crops', 'rice', 'corn', 'wetlands'))) %>%
  ggplot(aes(year, area/1000, color = habitat)) + 
  geom_line(size = 1.5) + geom_point(size = 3) + 
  scale_color_manual(values = pointblue.palette[c(2, 3, 11, 4)], name = 'land cover') +
  ylab('total area (ha, thousands)') + xlab(NULL) +
  scale_y_continuous(limits = c(0, 900), expand = c(0, 0)) +
  # add average lines from CVJV
  geom_hline(data = cvjv, aes(yintercept = area/scale, color = habitat), 
             linetype = 'dashed', size = 0.75) + 
  theme_presentation
ggsave(here::here(plot_baseline), 
       height = ppt.height, width = ppt.width.wide, 
       units = 'in', dpi = 400)

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
                        levels = c('2013-14', '2014-15', '2015-16', '2016-17'))) %>%
  rename(year = group)

floodpred %>%
  ggplot(aes(yday, fit)) + 
  facet_wrap(~habitat, ncol = 1) + 
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = year), alpha = 0.4) +
  geom_line(aes(color = year), size = 1) +
  scale_fill_viridis_d(name = 'water year') + 
  scale_color_viridis_d(name = 'water year') +
  ylim(0, 1) + ylab('Proportion open water') + 
  theme_presentation + timeaxis + timetheme
ggsave(here::here(plot_floodcurves), 
       height = ppt.height * 1.75, width = ppt.width.wide, 
       units = 'in', dpi = 400)


# HABITAT AVAILABILITY------------------
scale = 1000

habitat_avail <- read_csv(here::here(habitat_daily), col_types = cols()) %>%
  # lump incentive programs together, and wetland types together
  mutate(incentives = br_fall + br_spring + whep_fall + whep_vardd,
         wetlands = seas + perm) %>%
  select(group, time, watertype, corn, other, rice, incentives, wetlands) %>%
  gather(corn:wetlands, key = 'habitat', value = 'value') %>%
  mutate(habitat = factor(habitat, levels = levels),
         habitat = recode(habitat, 
                          other = 'other crops'))

peaks <- read_csv(here::here(habitat_stats), col_types = cols()) %>%
  mutate(incentives = recode(incentives, 
                             all = 'with incentives', 
                             none = 'without'))

# -- open water by year:

ggplot(habitat_avail %>% filter(watertype == 'open')) + 
  facet_wrap(~group, ncol = 1) +
  geom_area(aes(time, value/scale, fill = habitat)) +
  scale_fill_manual(values = pal, name = 'land cover') +
  ylab('Open water (ha, thousands)') +
  # add line for peak value in each year
  geom_vline(data = peaks %>% filter(watertype == 'open'),
             aes(xintercept = time, linetype = incentives), size = 1) +
  scale_linetype(name = 'peak values') +
  theme_presentation + timeaxis + timetheme
ggsave(here::here(plot_habitat_open), 
       height = ppt.height * 1.75, width = ppt.width.wide, 
       units = 'in', dpi = 400)


# -- accessible water by year:

ggplot(habitat_avail %>% filter(watertype == 'accessible')) + 
  facet_wrap(~group, nrow = 4) +
  geom_area(aes(time, value/scale, fill = habitat)) +
  scale_fill_manual(values = pal, name = 'land cover') +
  ylab('Accessible open water (ha, thousands)') +
  # add line for peak value in each year
  geom_vline(data = peaks %>% filter(watertype == 'accessible'), 
             aes(xintercept = time, linetype = incentives), size = 1) +
  scale_linetype(name = 'peak values') +
  theme_presentation + timeaxis + timetheme
ggsave(here::here(plot_habitat_accessible), 
       height = ppt.height * 1.75, width = ppt.width.wide, 
       units = 'in', dpi = 400)

# summary table of differences in peak values:
peaks %>% 
  select(-time) %>% 
  spread(key = incentives, value = area) %>% 
  mutate(percent_change = `with incentives`/without * 100 - 100) %>% 
  arrange(watertype)

# -- total hectare-days by land cover type
total_habitat <- habitat_avail %>%
  group_by(watertype, group, habitat) %>% 
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(total = sum(value),
         prop = value / total) %>%
  ungroup() %>%
  arrange(watertype, habitat, group) %>%
  mutate(group = factor(group, levels = rev(c('2013-14', '2014-15', '2015-16', '2016-17'))),
         watertype = factor(watertype, levels = c('open', 'accessible')),
         watertype = recode(watertype, open = 'Open water', accessible = 'Accessible open water'))
# print(total_habitat, n = 40)

ggplot(total_habitat) +
  facet_wrap(~watertype, ncol = 1) +
  geom_col(aes(x = group, y = value/1000000, fill = habitat)) +
  scale_fill_manual(values = pal, name = 'landcover') + 
  labs(x = NULL, y = 'Total hectare-days (millions)') +
  theme_presentation +
  coord_flip()
ggsave(here::here(plot_habitat_proportions), 
       height = ppt.height, width = ppt.width.wide, 
       units = 'in', dpi = 400)


# ENERGY CONSUMED----------------
scale = 1000000

consumed <- read_csv(here::here(results_energy_consumed), col_types = cols()) %>%
  mutate(incentive_scenario = incentives,
         incentives = br_fall + br_spring + whep_fall + whep_vardd,
         wetlands = seas + perm) %>%
  select(scenario:time, incentive_scenario, population, corn:other, rice, 
         incentives, wetlands) %>%
  gather(corn:wetlands, key = 'habitat', value = 'value') %>%
  left_join(read_csv(here::here(results_energy), col_types = cols()) %>% 
              select(scenario:DER),
            by = c('scenario', 'group', 'time')) %>%
  mutate(habitat = factor(habitat, levels = levels),
         group = factor(group, levels = rev(c('2013-14', '2014-15', '2015-16', '2016-17'))))

total_consumed <- consumed %>% 
  group_by(scenario, group, population, incentive_scenario, habitat) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  mutate(total = sum(value),
         prop = value / total) %>%
  ungroup() %>%
  mutate(habitat = recode(habitat, other = 'other crops'),
         incentive_scenario = recode(incentive_scenario,
                                     all = 'with incentives',
                                     none = 'without'))

# total energy consumed by land cover type 
ggplot(total_consumed %>% filter(scenario == 'obj_det'), 
       aes(x = group, y = value/1000000000, fill = habitat)) + 
  geom_col() +
  scale_fill_manual(values = pal, name = 'land cover') +
  labs(x = NULL, y = 'Total energy consumed (kJ, billions)') + 
  scale_y_continuous(expand = c(0,0)) +
  theme_presentation + 
  coord_flip()
ggsave(here::here(plot_energy_consumed), 
       height = ppt.height * 0.67, width = ppt.width.wide, units = 'in')


# total proportion of energy consumed in incentivized acres
total_consumed %>% filter(habitat == 'incentives' & scenario == 'obj_det')

# impact of incentive programs on energy consumed in wetlands (reduce by <2%)
total_consumed %>% 
  ungroup() %>%
  filter(habitat == 'wetlands' & scenario %in% c('obj_det', 'obj_det2')) %>%
  select(-total, -prop, -incentive_scenario) %>%
  mutate(scenario = recode(scenario, obj_det = 'all', obj_det2 = 'none')) %>%
  spread(key = scenario, value = value) %>% 
  mutate(prop_all = (none - all)/none * 100)


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
  theme_presentation + timeaxis + timetheme +
  geom_line(aes(y = DER/scale), size = 1)
ggsave(here::here(plot_energy_shortfall), 
       height = ppt.height * 1.75, width = ppt.width.wide, units = 'in', dpi = 400)

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
  theme_presentation + timeaxis + timetheme +
  geom_line(aes(y = DER/scale), size = 1)
ggsave(here::here(plot_energy_shortfall_baseline), 
       height = ppt.height * 1.75, width = ppt.width.wide, units = 'in', dpi = 400)

# % reduction in seasonal shortfalls contributed by incentive programs
reduction <- energy %>% 
  left_join(habitat_avail %>% 
              filter(watertype == 'open' & habitat=='incentives') %>%
              select(group, time, incentive_ha = value), 
            by = c('group', 'time')) %>%
  # before Oct 1 and over Nov 30
  mutate(season = case_when(time <= 92 ~ 'fall',
                            time > 153 ~ 'spring',
                            TRUE ~ 'winter')) %>%
  group_by(scenario, group, incentives, population, season) %>%
  summarize(shortfall = sum(shortfall),
            incentive_ha = sum(incentive_ha)) %>%
  ungroup() %>%
  select(-scenario) %>%
  # gather(shortfall:value, key = 'var', value = 'value') %>%
  filter(incentives != 'fall only') %>%
  mutate(incentives = paste0('shortfall_', incentives)) %>%
  spread(key = incentives, value = shortfall) %>%
  mutate(diff = shortfall_none - shortfall_all,
         perc = diff / shortfall_none * 100,
         ratio = diff / incentive_ha, #calories per hectare-day
         perc.ratio = perc / incentive_ha) %>% #% per hectare-day) %>%
  arrange(population, group, season)
write_csv(reduction, here::here(energy_shortfall_reduction))

br_ts <- 'data/BR_timeseries.csv'
whep_ts <- 'data/WHEP_timeseries.csv'
incentives <- bind_rows(read_csv(here::here(br_ts), col_types = cols()), 
                        read_csv(here::here(whep_ts), col_types = cols())) %>%
  rename(openwater = available, time = yday) %>%
  gather(openwater:prop.accessible, key = 'var', value = 'value') %>%
  mutate(var = factor(var, 
                      levels = c('openwater', 'added', 'returned', 'accessible', 'prop.accessible'))) %>%
  spread(key = 'habitat', value = 'value') %>%
  arrange(group, var, time) %>%
  filter(var == 'openwater')

reduction %>%
  select(group, population, season, diff, perc) %>%
  ggplot(aes(group, perc, fill = season)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~population, ncol = 1) +
  scale_fill_manual(values = pointblue.palette[c(2,4)]) +
  labs(x = NULL, y = '% reduction in energy shortfalls') + 
  theme_presentation +
  # theme_minimal() + theme(strip.text = element_text(hjust = 0)) +
  ylim(0, 100)
ggsave(here::here(plot_energy_effect), 
       height = ppt.height, width = ppt.width, units = 'in', dpi = 400)
