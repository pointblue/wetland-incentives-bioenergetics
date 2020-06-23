##' @title Plot results
##' @param coverdf data.frame containing annual estimates of each land cover
##'   type
##' @param flooddf data.frame containing daily estimates of proportion open
##'   water by land cover type and water year
##' @param habitatdf data.frame containing daily estimates of the area of open
##'   water by land cover type and water year, for both total open water and
##'   accessible water
##' @param energydf data.frame resulting from run_bioenergmod_loop containing
##'   daily estimates of energy shortfalls
##' @param scale value by which to divide the y-axis
##' @param ymax maximum value of the y-axis
##' @param ylab character string containing label(s) for the y-axis
##' @param filename path to output file (see ggsave)
##' @param width width of plot in mm
##' @param height height of plot in mm
##' @param dpi dpi of plot
##' @description Functions to produce publication-ready summaries of results

# CUSTOM DESIGN------------
pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666', '#456d28', 
                       #add a few more complementary colors
                       '#b74374', '#5e2a84', '#d2c921')

pal <- c(pointblue.palette[c(9, 3)], 'yellow', pointblue.palette[c(2, 4)])

# theme for manuscript figures (smaller font sizes)
theme_manuscript <- theme_classic() + 
  theme(title = element_text(size = 10),
        plot.title = element_text(size = 10, face = 'bold'), 
        axis.text = element_text(size = 8, color = 'black'),
        legend.text = element_text(size = 8, lineheight = 1),
        legend.background = element_blank(),
        legend.key.size = unit(0.3, 'cm'),
        strip.background = element_rect(color = NA),
        strip.text = element_text(hjust = 0, face = 'plain', size = 8),
        legend.justification = 'top')


# theme for monthly time series (add grid lines and ticks)
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

timetheme <- theme(
  panel.grid.major.x = element_line(
    color = c(rep(c('gray90', NA), length(timeaxis[[1]]$breaks - 1)/2), 'gray90'),
    size = 0.3),
  axis.ticks.x = element_line(
    color = c(rep(c('black', NA), length(timeaxis[[1]]$breaks - 1)/2), 'black')))


plot_landcover <- function(coverdf, scale = 1000, ymax = 750,
                           palette = c('gray20', 'gray40', 'gray60', 'black'),
                           ylab = 'Total area (ha, thousands)',
                           filename, width = 80, height = 60, dpi = 400) {
  coverdf <- coverdf %>% filter(!habitat %in% c('seas', 'perm')) %>%
    mutate(habitat = recode(habitat, other = 'other crops'),
           habitat = factor(habitat,
                            levels = c('other crops', 'rice', 'corn', 'wetlands')))

  coverdf %>% ggplot(aes(year, ha/scale, color = habitat)) +
    geom_line() + geom_point(size = 1) +
    scale_color_manual(values = palette, name = 'land cover') +
    labs(x = NULL, y = ylab, title = NULL) +
    scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
    geom_text(data = coverdf %>% filter(year == 2013),
              aes(year, y = ha/scale + c(30, 30, 30, -30), label = habitat),
              hjust = 0, color = 'black', size = 3) +
    theme_manuscript + theme(legend.position = 'none')

  ggsave(filename, height = height, width = width, units = 'mm', dpi = dpi)
}
  
plot_floodcurves <- function(flooddf, filename, width = 80, height = 140, 
                             dpi = 400) {
  flooddf <- flooddf %>% filter(habitat != 'other') %>%
    mutate(habitat = factor(habitat, levels = c('wetlands', 'rice', 'corn')),
           yeartype = recode(group,
                             '2013-14' = 'critically dry',
                             '2014-15' = 'critically dry',
                             '2015-16' = 'below normal',
                             '2016-17' = 'wet'),
           label = recode(habitat,
                          'wetlands' = 'A',
                          'rice' = 'B',
                          'corn' = 'C',
                          'other crops' = 'D')) %>%
    rename(year = group)
  
  flooddf %>% ggplot(aes(yday, fit)) +
    facet_wrap(~label, ncol = 1) +
    geom_line(aes(linetype = year, color = year)) +
    geom_hline(aes(yintercept = 0)) +
    scale_color_manual(values = c('gray70', 'gray70', 'gray40', 'black')) +
    scale_linetype_manual(values = c('dashed', 'dotted', 'longdash', 'solid')) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    labs(y = 'Proportion open water', color = NULL, linetype = NULL) +
    theme_manuscript + timeaxis + timetheme +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(hjust = 0, face = 'bold', size = 10))
  
  ggsave(filename, height = height, width = width, units = 'mm', dpi = dpi)

}

plot_habitat <- function(habitatdf, scale = 1000, ymax = 250,
                         levels = c('incentives', 'rice', 'corn', 'other', 
                                    'wetlands'),
                         ylabs = c('Open water (ha, thousands)',
                                   'Accessible open water (ha, thousands)'),
                         filename, width = 169, height = 180, dpi = 400) {
  habitatdf <- habitatdf %>%
    select(group, time, watertype, wetlands, rice, corn, other, incentives) %>%
    gather(wetlands:incentives, key = 'habitat', value = 'value') %>%
    mutate(habitat = factor(habitat, levels = levels),
           habitat = recode(habitat, other = 'other crops'))

  open <- habitatdf %>% filter(watertype == 'open') %>%
    ggplot() + facet_wrap(~group, ncol = 1) +
    geom_area(aes(time, value/scale, fill = habitat)) +
    scale_fill_manual(values = pal, name = NULL) +
    labs(x = NULL, y = ylabs[1], title = 'A') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme +
    theme(legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          panel.grid.major.x = element_blank())

  accessible <- habitatdf %>% filter(watertype == 'accessible') %>%
    ggplot() + facet_wrap(~group, ncol = 1) +
    geom_area(aes(time, value/scale, fill = habitat)) +
    scale_fill_manual(values = pal, name = NULL) +
    labs(x = NULL, y = ylabs[2], title = 'B') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme +
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank())

  require(patchwork)
  open + accessible
  ggsave(filename, height = height, width = width, units = 'mm', dpi = dpi)

}

plot_shortfalls <- function(energydf, habitatdf, scale = 1000000, ymax = 250, 
                            fillpalette = c('without' = 'gray80', 
                                            'with' = 'gray50'),
                            segmentpalette = c('black', 'gray60'),
                            ylab = 'Energy shortfall (kJ, millions)',
                            filename, width = 169, height = 180, dpi = 400) {
  energydf <- energydf %>% 
    mutate(incentives = factor(incentives, levels = c('without', 'with')))
  
  # get incentive timing
  habitatdf <- habitatdf %>% 
    filter(watertype == 'open') %>%  
    select(group, time, br_fall:whep_vardd) %>% 
    pivot_longer(br_fall:whep_vardd) %>% 
    filter(value > 0) %>% 
    group_by(group, name) %>% 
    summarize(start = min(time),
              end = max(time),
              .groups = 'drop')
  
  base <- energydf %>% filter(population == 'baseline') %>%
    ggplot(aes(time, shortfall/scale)) +
    facet_wrap(~group, ncol = 1) +
    geom_area(aes(fill = incentives), position = position_identity()) +
    scale_fill_manual(values = fillpalette) +
    labs(x = NULL, y = ylab, title = 'A') +
    scale_y_continuous(expand = c(0,0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme + theme(legend.position = 'none') +
    geom_segment(data = habitatdf %>%
                   filter(name %in% c('br_fall', 'br_spring')),
                 aes(x = start, xend = end, y = .98*ymax, yend = .98*ymax),
                 color = segmentpalette[1],
                 arrow = arrow(angle = 90, ends = 'both',
                               length = unit(0.02, 'inches'))) +
    geom_segment(data = habitatdf %>%
                   filter(name %in% c('whep_fall', 'whep_vardd')),
                 aes(x = start, xend = end, y = .94*ymax, yend = .94*ymax),
                 color = segmentpalette[2], linetype = 'longdash',
                 arrow = arrow(angle = 90, ends = 'both',
                               length = unit(0.02, 'inches')))
  
  obj <- energydf %>% filter(population == 'objectives') %>%
    ggplot(aes(time, shortfall/scale)) +
    facet_wrap(~group, ncol = 1) +
    geom_area(aes(fill = incentives), position = position_identity()) +
    scale_fill_manual(values = fillpalette) +
    labs(x = NULL, y = NULL, title = 'B') +
    scale_y_continuous(expand = c(0,0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme + theme(legend.position = 'none') +
    geom_segment(data = habitatdf %>%
                   filter(name %in% c('br_fall', 'br_spring')),
                 aes(x = start, xend = end, y = .98*ymax, yend = .98*ymax),
                 color = segmentpalette[1],
                 arrow = arrow(angle = 90, ends = 'both',
                               length = unit(0.02, 'inches'))) +
    geom_segment(data = habitatdf %>%
                   filter(name %in% c('whep_fall', 'whep_vardd')),
                 aes(x = start, xend = end, y = .94*ymax, yend = .94*ymax),
                 color = segmentpalette[2], linetype = 'longdash',
                 arrow = arrow(angle = 90, ends = 'both',
                               length = unit(0.02, 'inches')))
  
  require(patchwork)
  base + obj
  ggsave(filename, height = height, width = width, units = 'mm', dpi = dpi)

}
