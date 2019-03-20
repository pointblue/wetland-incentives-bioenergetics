# README----------
# Stub for script to produce publication and presentation-ready plots

# PACKAGES
library(tidyverse)

# INPUTS
floodcurves <- 'output/open_water_annual.csv'

# OUTPUTS
plot_floodcurves <- 'figs/flood_curves.png'


# FLOODING CURVES-------
theme = theme_classic() + 
  theme(panel.grid.major = element_line(color = 'gray90'),
        strip.text = element_text(hjust = 0),
        strip.background = element_blank())

axes = list(scale_x_continuous(breaks = c(1, 32, 63, 93, 124, 154, 185, 216, 244, 275, 305), 
                               labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                          'Feb', 'Mar', 'Apr', 'May')),
            xlab(NULL),
            ylab('Proportion open water'))

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
  ylim(0, 1) + axes + theme 
ggsave(here::here(plot_floodcurves), width = 6.5, height = 7.5, units = 'in')
