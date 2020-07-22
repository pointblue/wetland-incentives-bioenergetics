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

plot_shortfalls <- function(energydf, habitatdf, needspath,
                            scale = 1000000, ymax = 250, 
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
  
  # get energy needs
  needs <- read_csv(needspath, col_types = cols())
  
  base <- energydf %>% filter(population == 'baseline') %>%
    ggplot(aes(time, shortfall/scale)) +
    facet_wrap(~group, ncol = 1, scales = 'free_x') +
    geom_area(aes(fill = incentives), position = position_identity()) +
    scale_fill_manual(values = fillpalette) +
    geom_line(data = needs, aes(yday, DER.obs/scale), size = 0.3) +
    labs(x = NULL, y = ylab, title = 'A') +
    scale_y_continuous(expand = c(0,0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme + 
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank()) +
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
    facet_wrap(~group, ncol = 1, scales = 'free_x') +
    geom_area(aes(fill = incentives), position = position_identity()) +
    scale_fill_manual(values = fillpalette) +
    geom_line(data = needs, aes(yday, DER.obj/scale), size = 0.3) +
    labs(x = NULL, y = NULL, title = 'B') +
    scale_y_continuous(expand = c(0,0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme + 
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank()) +
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

plot_shortfalls_ci <- function(energydf, needspath,
                               scale = 1000000, ymax = 250, 
                               ylab = 'Energy shortfall (kJ, millions)',
                               filename, width = 169, height = 180, dpi = 400) {
  # get energy needs
  needs <- read_csv(needspath, col_types = cols())
  
  base <- energydf %>% filter(population == 'baseline' & incentives == 'without') %>%
    ggplot(aes(time, total/scale)) +
    facet_wrap(~group, ncol = 1, scales = 'free_x') +
    geom_ribbon(aes(ymin = lcl/scale, ymax = ucl/scale), fill = 'gray80') +
    geom_line() +
    geom_line(data = needs, aes(yday, DER.obs/scale), size = 0.3, linetype = 'dashed') +
    labs(x = NULL, y = ylab, title = 'A') +
    scale_y_continuous(expand = c(0,0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme + 
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank())
  
  obj <- energydf %>% filter(population == 'objectives' & incentives == 'without') %>%
    ggplot(aes(time, total/scale)) +
    facet_wrap(~group, ncol = 1, scales = 'free_x') +
    geom_ribbon(aes(ymin = lcl/scale, ymax = ucl/scale), fill = 'gray80') +
    geom_line() +
    geom_line(data = needs, aes(yday, DER.obj/scale), size = 0.3, linetype = 'dashed') +
    labs(x = NULL, y = NULL, title = 'B') +
    scale_y_continuous(expand = c(0,0), limits = c(0, ymax)) +
    theme_manuscript + timeaxis + timetheme + 
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank())
  
  require(patchwork)
  base + obj
  ggsave(filename, height = height, width = width, units = 'mm', dpi = dpi)
  
}

plot_shortfall_timeline <- function(energydf, interval = 'day', size = 16,
                                    filename, height, width, dpi) {
  if (interval == 'day') {
    energylab <- energydf %>% filter(incentives == 'without') %>% 
      group_by(scenario, population, time) %>% 
      summarize(max = max(shortfall),
                min = min(shortfall),
                .groups = 'drop') %>% 
      mutate(label = case_when(min > 0 ~ 'consistent',
                               max == 0 ~ 'none',
                               TRUE ~ 'sometimes'))
    
    repeats <- rle(energylab$label)
    
    energylab <- energylab %>% 
      mutate(segs = rep(paste0('seg', c(1:length(repeats$lengths))), 
                        times = repeats$lengths),
             population = factor(population, 
                                 levels = c('objectives', 'baseline')),
             population = recode(population, baseline = 'baseline\npopulation',
                                 objectives = 'population\nobjectives')) %>% 
      group_by(scenario, population, segs, label) %>% 
      summarize(start = min(time),
                end = max(time)+1,
                .groups = 'drop') %>% 
      arrange(scenario, population, start)
    
  } else if (interval == 'week') {
    
    energylab <- energydf %>% filter(incentives == 'without') %>% 
      mutate(week = rep(c(rep(paste0('week', c(1:45)), each = 7), 
                          rep('week46', 4)), 
                        8)) %>%
      group_by(scenario, population, group, week) %>% 
      summarize(start = min(time),
                end = max(time),
                shortfall = sum(shortfall), .groups = 'drop') %>% 
      group_by(scenario, population, week, start, end) %>%
      summarize(max = max(shortfall),
                min = min(shortfall),
                .groups = 'drop') %>%
      mutate(label = case_when(min > 0 ~ 'consistent',
                               max == 0 ~ 'none',
                               TRUE ~ 'sometimes'))
    
    repeats <- rle(energylab$label)
    
    energylab <- energylab %>% 
      mutate(segs = rep(paste0('seg', c(1:length(repeats$lengths))), 
                        times = repeats$lengths),
             population = factor(population, 
                                 levels = c('objectives', 'baseline')),
             population = recode(population, baseline = 'baseline\npopulation',
                                 objectives = 'population\nobjectives')) %>% 
      group_by(scenario, population, segs, label) %>% 
      summarize(start = min(start),
                end = max(end)+1,
                .groups = 'drop') %>% 
      arrange(scenario, population, start)
    
  } else if (interval == 'week_ci') {
    
    energylab <- energydf %>% filter(incentives == 'without') %>% 
      mutate(week = rep(c(rep(paste0('week', c(1:45)), each = 7), 
                          rep('week46', 4)), 
                        8)) %>% 
      group_by(scenario, population, group, week) %>% 
      summarize(start = min(time), 
                end = max(time), 
                total = sum(total), 
                lcl = sum(lcl), 
                ucl = sum(ucl), 
                .groups = 'drop') %>% 
      mutate(label = case_when(total > 0 & lcl > 0 ~ 'g97', 
                               total > 0 & lcl == 0 ~ 'g50', 
                               total == 0 & ucl > 0 ~ 'g2', 
                               total == 0 & ucl == 0 ~ 'l2')) %>% 
      group_by(scenario, population, week, start, end, label) %>% 
      count() %>% 
      ungroup() %>% 
      pivot_wider(names_from = 'label', values_from = 'n') %>% 
      arrange(population, start) %>% 
      mutate(label = case_when(g97 == 4 ~ 'always', 
                               g50 + g97 == 4 ~ 'consistent', 
                               g50 == 4 ~ 'consistent', 
                               g50 < 4 & is.na(g97) ~ 'sometimes',
                               l2 == 4 ~ 'none', 
                               TRUE ~ 'uncertain'))
    
    repeats <- rle(energylab$label)
    
    energylab <- energylab %>% 
      mutate(segs = rep(paste0('seg', c(1:length(repeats$lengths))), 
                        times = repeats$lengths),
             population = factor(population, 
                                 levels = c('objectives', 'baseline')),
             population = recode(population, baseline = 'baseline\npopulation',
                                 objectives = 'population\nobjectives')) %>% 
      group_by(scenario, population, segs, label) %>% 
      summarize(start = min(start),
                end = max(end)+1,
                .groups = 'drop') %>% 
      arrange(scenario, population, start)
    
  } else if (interval == 'halfmonth') {
    
    energylab <- energydf %>% filter(incentives == 'without') %>% 
      mutate(halfmonth = rep(rep(c('Jul1-15', 'Jul16-31', 'Aug1-15', 'Aug16-31',
                                   'Sep1-15', 'Sep16-30', 'Oct1-15', 'Oct16-31',
                                   'Nov1-15', 'Nov16-30', 'Dec1-15', 'Dec16-31',
                                   'Jan1-15', 'Jan16-31', 'Feb1-15', 'Feb16-28',
                                   'Mar1-15', 'Mar16-31', 'Apr1-15', 'Apr16-30',
                                   'May1-15'),
                                 times = c(15, 16, 15, 16, 15, 15, 15, 16,
                                           15, 15, 15, 16, 15, 16, 15, 13,
                                           15, 16, 15, 15, 15)), 8),
             halfmonth = factor(halfmonth,
                                levels = c('Jul1-15', 'Jul16-31', 'Aug1-15', 'Aug16-31',
                                           'Sep1-15', 'Sep16-30', 'Oct1-15', 'Oct16-31',
                                           'Nov1-15', 'Nov16-30', 'Dec1-15', 'Dec16-31',
                                           'Jan1-15', 'Jan16-31', 'Feb1-15', 'Feb16-28',
                                           'Mar1-15', 'Mar16-31', 'Apr1-15', 'Apr16-30',
                                           'May1-15'))) %>%
      group_by(scenario, population, group, halfmonth) %>% 
      summarize(start = min(time),
                end = max(time),
                shortfall = sum(shortfall), .groups = 'drop') %>% 
      group_by(scenario, population, halfmonth, start, end) %>%
      summarize(max = max(shortfall),
                min = min(shortfall),
                .groups = 'drop') %>%
      mutate(label = case_when(min > 0 ~ 'consistent',
                               max == 0 ~ 'none',
                               TRUE ~ 'sometimes'))
    
    repeats <- rle(energylab$label)
    
    energylab <- energylab %>% 
      mutate(segs = rep(paste0('seg', c(1:length(repeats$lengths))), 
                        times = repeats$lengths),
             population = factor(population, 
                                 levels = c('objectives', 'baseline')),
             population = recode(population, baseline = 'baseline\npopulation',
                                 objectives = 'population\nobjectives')) %>% 
      group_by(scenario, population, segs, label) %>% 
      summarize(start = min(start),
                end = max(end)+1,
                .groups = 'drop') %>% 
      arrange(scenario, population, start)
  }
  
  if ('always' %in% energylab$label) {
    palette = c('always' = 'black',
                'consistent' = 'gray35',
                'sometimes' = 'gray65',
                'uncertain' = 'gray80',
                'none' = 'gray80')
  } else {
    palette = c('consistent' = 'black',
                'sometimes' = 'gray50',
                'none' = 'gray80')
  }
  ggplot(energylab, 
         aes(x = start, xend = end, 
             y = population, yend = population, 
             color = label)) +
    geom_segment(linetype = 1, size = size) +
    scale_color_manual(values = palette, name = NULL) +
    labs(y = NULL, x = NULL) +
    timeaxis + theme_manuscript + timetheme + 
    theme(legend.position = 'none',
          panel.grid.major.x = element_blank())
  
  ggsave(filename, height = height, width = width, units = 'mm', dpi = dpi)
}

plot_filled_habitat <- function(filled, scale = 1000, ymax = 100,
                                ylab = 'Additional habitat needed (ha, thousands)',
                                filename, height, width, units, dpi) {
  filled %>% select(population:time, new) %>% 
    group_by(population, time) %>% 
    summarize(min = min(new), max = max(new), .groups = 'drop') %>% 
    pivot_longer(min:max) %>% 
    mutate(population = recode(population, baseline = 'A', objectives = 'B')) %>% 
    ggplot(aes(time, value/scale, fill = name)) + 
    geom_area(position = 'identity') + 
    facet_wrap(~population, ncol = 1) + 
    scale_fill_manual(values = c('gray50', 'black')) + 
    theme_manuscript + timetheme + timeaxis +
    labs(y = ylab) + ylim(0, ymax) +
    theme(legend.position = 'none')
  
  ggsave(filename, height = height, width = width, units = 'mm', dpi = dpi)
}

plot_incentive_effects <- function(energydf, consumptiondf, habitatdf, 
                                   scale = 1000000000, ymax = 25) {
  # left_join(consumptiondf %>% 
  #             # program-specific proportion of total energy consumed across all
  #             # incentive programs
  #             select(population:time, br_fall:whep_vardd) %>% 
  #             filter(incentives == 'with') %>% 
  #             pivot_longer(br_fall:whep_vardd) %>% 
  #             group_by(population, incentives, group, name) %>% 
  #             summarize(value = sum(value), .groups = 'drop') %>% 
  #             group_by(population, incentives, group) %>% 
  #             mutate(total = sum(value), prop = value/total),
  #           energydf %>% 
  #             # difference in total energy shortfalls with vs without incentive
  #             # programs
  #             select(population:time, shortfall) %>% 
  #             group_by(population, group, incentives) %>% 
  #             summarize(shortfall = sum(shortfall), .groups = 'drop') %>% 
  #             pivot_wider(names_from = incentives, values_from = shortfall) %>% 
  #             mutate(diff = without - with) %>% 
  #             select(population, group, without, diff),
  #           by = c('population', 'group')) %>% 
  #   # assume contribution of each program to reduction in energy shortfalls is
  #   # proportional to energy consumed in each land cover type
  #   mutate(contribution = prop * diff,
  #          contribution.perc = contribution / without) %>% 
  #   # match to total number of hectare-days flooded (including drawdown
  #   # period)
  #   left_join(habitatdf %>% filter(watertype == 'open') %>% 
  #               select(group, time, br_fall:whep_vardd) %>% 
  #               pivot_longer(br_fall:whep_vardd) %>% 
  #               group_by(group, name) %>% 
  #               summarize(investment = sum(value), .groups = 'drop'),
  #             by = c('group', 'name')) %>% 
  #   # calculate contribution to energy reduction per ha-day
  #   mutate(ratio = contribution/investment,
  #          ratio.perc = contribution.perc/investment*100000,
  #          name = factor(name, 
  #                        levels = c('whep_fall', 'br_fall', 'whep_vardd', 'br_spring'))) %>% 
  #   ggplot(aes(name, ratio.perc, fill = group)) + geom_col(position = 'dodge') +
  #   scale_fill_manual(values = pointblue.palette[c(1:3,5)]) +
  #   facet_wrap(~population, ncol = 1, scales = 'free_y') +
  #   labs(x = 'incentive program', 
  #        y = '% reduction in total energy shortfall per 100k ha-days') +
  #   theme_manuscript + 
  #   theme(legend.position = c(1, 1), 
  #         legend.justification = c(1,1))
  
  # consumption bar chart:
  # left_join(consumed, energysum, 
  #           by = c('scenario', 'population', 'incentives', 'group', 'time')) %>% 
  #   mutate(wetlands = seas + perm) %>% 
  #   select(-seas, -perm) %>% 
  #   group_by(scenario, population, incentives, group) %>% 
  #   summarize_at(vars(corn:wetlands), ~sum(., na.rm = TRUE)) %>% 
  #   pivot_longer(cols = c(corn:whep_vardd, shortfall, wetlands)) %>% 
  #   mutate(name = factor(name, 
  #                        levels = c('shortfall', 'whep_fall', 
  #                                   'br_fall', 'br_spring', 'whep_vardd', 
  #                                   'rice', 'wetlands', 'corn', 'other'))) %>% 
  #   # pivot_wider(names_from = incentives, values_from = value) %>% 
  #   filter(population == 'objectives') %>% 
  #   ggplot() + 
  #   geom_col(aes(incentives, value, fill = name), position = 'fill') + 
  #   facet_wrap(~group, ncol = 4) + 
  #   scale_fill_manual(values = c('gray80', pointblue.palette[c(8:11, 3)], 
  #                                pointblue.palette[4], 'yellow', pointblue.palette[2])) + 
  #   scale_y_continuous(expand = c(0,0)) +
  #   theme_manuscript
  
  # old version:
  effect <- left_join(energydf %>% 
                        mutate(season = ifelse(time <= 123, 'fall', 'spring')) %>% 
                        group_by(population, incentives, group, season) %>% 
                        summarize(shortfall = sum(shortfall), .groups = 'drop') %>% 
                        pivot_wider(names_from = incentives, values_from = shortfall),
                      habitatdf %>% filter(watertype == 'open') %>%
                        mutate(incentive_fall = br_fall + whep_fall,
                               incentive_spring = br_spring + whep_vardd) %>%
                        select(group, time, incentive_fall:incentive_spring) %>% 
                        pivot_longer(incentive_fall:incentive_spring, 
                                     values_to = 'ha_days') %>% 
                        separate(name, into = c('incentive', 'season')) %>% 
                        group_by(group, season) %>% 
                        summarize(ha_days = sum(ha_days), .groups = 'drop'),
                      by = c('group', 'season')) %>%
    mutate(diff = without - with,
           diff.perc = diff / without * 100,
           # absolute reduction hectare-day incentivized
           ratio = diff / ha_days,
           # % reduction per 1 million hectare-days)
           ratio.perc = diff.perc / ha_days * 10000) %>%
    arrange(population, group)
  
  effect %>% 
    ggplot(aes(season, ratio, fill = group)) +
    geom_col(position = position_dodge()) +
    facet_wrap(~population, ncol = 1) +
    # scale_fill_manual(values = pointblue.palette[c(2,4)]) +
    scale_fill_grey() +
    labs(x = NULL, y = 'Reduction in energy shortfalls (kJ) per ha-day', title = NULL) +
    scale_y_continuous(limits = c(0, ymax), expand = c(0, 0)) +
    theme_manuscript +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.title = element_blank())
  # theme(legend.position = 'none')
  
  # absdiff <- effect %>% select(group, population, diff) %>%
  #   ggplot(aes(group, diff/scale, fill = population)) +
  #   geom_col(position = position_dodge()) +
  #   scale_fill_manual(values = pointblue.palette[c(2,4)]) +
  #   labs(x = NULL, y = 'Reduction in shortfalls (kJ, billions)', title = 'A') +
  #   scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
  #   theme_manuscript +
  #   theme(legend.position = c(1, 1), legend.justification = c(1, 1),
  #         legend.title = element_blank())
  
  # #    -- incentive ha (MS version)
  # invest <- effect %>% select(group, incentive_ha) %>% distinct() %>%
  #   ggplot(aes(group, incentive_ha/1000000)) + geom_col(fill = 'gray50', width = 0.5) +
  #   labs(x = NULL, y = 'ha-days incentivized (millions)', title = 'C') +
  #   scale_y_continuous(limits = c(0, 3), expand = c(0, 0)) +
  #   theme_manuscript +
  #   theme(legend.position = 'none')
  #
  # absdiff/percdiff/invest
  # ggsave(here::here(plot_energy_effect_ms),
  #        height = ms.single * 2.25, width = ms.single,
  #        units = 'mm', dpi = 400)
  
}

