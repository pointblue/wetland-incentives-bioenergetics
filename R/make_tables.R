##' @title Make tables
##' @param effortdat data.frame containing annual estimates of hectares
##'   incentivized for each program
##' @param habitatdat data.frame containing the annual estimates of hectares of
##'   habitat available by land cover type, including lcl and ucl from Monte
##'   Carlo
##' @param shortfalltotals data.frame containing annual estimates of total
##'   energy shortfalls, including lcl and ucl from Monte Carlo
##' @param shortfallseason data.frame containing annual and seasonal estimates
##'   of total energy shortfalls, including lcl and ucl from Monte Carlo
##' @param scale factor by which to divide values in the table
##' @param pathout name of filepath to be passed to save_as_docx
##' @description Functions to produce near-publication-ready tables of results

make_effort_table <- function(effortdat, scale = 1000000, pathout) {
  tabledat <- bind_rows(effortdat %>% filter(label == 'total') %>% select(-label) %>%
                          pivot_longer(br_fall:incentives, names_to = 'habitat',
                                       values_to = 'total') %>%
                          mutate(type = 'incentivized'),
                        accessible_mc %>%
                          filter(habitat %in% c('br_fall', 'br_spring', 'whep_fall',
                                                'whep_vardd', 'incentives')) %>%
                          mutate(type = 'accessible')) %>%
    select(type, group, habitat, total, lcl, ucl) %>%
    pivot_longer(total:ucl) %>%
    mutate(habitat = factor(habitat,
                            levels = c('br_fall', 'br_spring', 'whep_fall',
                                       'whep_vardd', 'incentives')),
           habitat = recode(habitat, incentives = 'total'),
           name = factor(name, levels = c('total', 'lcl', 'ucl', 'perc')),
           type = factor(type, levels = c('incentivized', 'accessible')),
           value = case_when(type == 'accessible' ~ value/scale,
                             TRUE ~ value)) %>%
    arrange(type, habitat, name) %>%
    unite(col = 'label', habitat, name) %>%
    pivot_wider(names_from = label, values_from = value) %>% 
    mutate_at(vars(ends_with('_total')),
              ~case_when(type == 'incentivized' ~ format(round(., digits = 0), big.mark = ','),
                         type == 'accessible' ~ format(round(., digits = 3), nsmall = 3)))
  
  table <- flextable(tabledat, 
                     col_keys = c('type', 'group', 
                                  'br_fall_total', 'br_fall_ci',
                                  'br_spring_total', 'br_spring_ci',
                                  ' ',
                                  'whep_fall_total', 'whep_fall_ci',
                                  'whep_vardd_total', 'whep_vardd_ci',
                                  'total_total', 'total_ci')) %>% 
    flextable::compose(j = "br_fall_ci",
                       value = as_paragraph('(',
                                            sprintf("%.03f", br_fall_lcl),
                                            "\U2012",
                                            sprintf("%.03f", br_fall_ucl), ')')) %>%
    flextable::compose(j = "br_spring_ci",
                       value = as_paragraph('(',
                                            sprintf("%.03f", br_spring_lcl),
                                            "\U2012",
                                            sprintf("%.03f", br_spring_ucl), ')')) %>%
    flextable::compose(j = "whep_fall_ci",
                       value = as_paragraph('(',
                                            sprintf("%.03f", whep_fall_lcl),
                                            "\U2012",
                                            sprintf("%.03f", whep_fall_ucl), ')')) %>%
    flextable::compose(j = "whep_vardd_ci",
                       value = as_paragraph('(',
                                            sprintf("%.03f", whep_vardd_lcl),
                                            "\U2012",
                                            sprintf("%.03f", whep_vardd_ucl), ')')) %>%
    flextable::compose(j = "total_ci",
                       value = as_paragraph('(',
                                            sprintf("%.03f", total_lcl),
                                            "\U2012",
                                            sprintf("%.03f", total_ucl), ')')) %>%
    set_header_labels(group = 'year',
                      br_fall_total = 'fall', 
                      br_fall_ci = 'fall',
                      br_spring_total = 'spring',
                      br_spring_ci = 'spring',
                      whep_fall_total = 'fall flooding',
                      whep_fall_ci = 'fall flooding',
                      whep_vardd_total = 'variable drawdown',
                      whep_vardd_ci = 'variable drawdown',
                      total_total = 'Total',
                      total_ci = 'Total') %>% 
    merge_at(i = 1, j = 3:4, part = 'header') %>% 
    merge_at(i = 1, j = 5:6, part = 'header') %>% 
    merge_at(i = 1, j = 8:9, part = 'header') %>% 
    merge_at(i = 1, j = 10:11, part = 'header') %>% 
    merge_at(i = 1, j = 12:13, part = 'header') %>% 
    add_header_row(values = c('type', 'year', 
                              'BirdReturns', 'BirdReturns',
                              'BirdReturns', 'BirdReturns', ' ',  
                              'WHEP', 'WHEP', 'WHEP', 'WHEP',
                              'Total', 'Total')) %>% 
    merge_at(i = 1, j = 3:6, part = 'header') %>% 
    merge_at(i = 1, j = 8:11, part = 'header') %>% 
    merge_at(j = 1, i = 1:2, part = 'header') %>% 
    merge_at(j = 2, i = 1:2, part = 'header') %>% 
    merge_at(j = 7, i = 1:2, part = 'header') %>% 
    merge_at(i = 1:2, j = 12:13, part = 'header') %>% 
    theme_vanilla() %>% 
    font(part = 'all', fontname = 'Times New Roman') %>% 
    fontsize(part = 'all', size = 12) %>% 
    bold(part = 'header') %>% 
    set_table_properties(width = 1, layout = 'autofit') %>% 
    # autofit(part = 'header') %>% 
    # autofit(add_w = 0, add_h = 0, part = 'body') %>% 
    align(align = 'center', part = 'header') %>% 
    align(align = 'left', part = 'body', j = c(4,6,9,11,13)) %>% 
    align(align = 'center', part = 'body', j = c(1, 2)) %>% 
    border_inner_h(border = officer::fp_border(width = 0))
  
  # table <- flextable(effort, 
  #                    col_keys = c('label', 'group', 
  #                                 'br_fall', 
  #                                 'br_spring', 
  #                                 ' ',
  #                                 'whep_fall', 
  #                                 'whep_vardd', 
  #                                 'incentives')) %>% 
  #   set_formatter(br_fall = function(x) format(round(x, digits = 0), 
  #                                              nsmall = 0, big.mark = ','),
  #                 br_spring = function(x) format(round(x, digits = 0), 
  #                                                nsmall = 0, big.mark = ','),
  #                 whep_fall = function(x) format(round(x, digits = 0), 
  #                                                nsmall = 0, big.mark = ','),
  #                 whep_vardd = function(x) format(round(x, digits = 0), 
  #                                                 nsmall = 0, big.mark = ','),
  #                 incentives = function(x) format(round(x, digits = 0), 
  #                                                 nsmall = 0, big.mark = ',')) %>% 
  #   set_header_labels(group = 'year',
  #                     br_fall = 'fall', 
  #                     br_spring = 'spring',
  #                     whep_fall = 'fall flooding',
  #                     whep_vardd = 'variable drawdown',
  #                     incentives = 'Total') %>% 
  #   add_header_row(values = c('label', 'year', 
  #                             'BirdReturns', 'BirdReturns', ' ',  
  #                             'WHEP', 'WHEP',
  #                             'Total')) %>% 
  #   merge_at(i = 1, j = 3:4, part = 'header') %>% 
  #   merge_at(i = 1, j = 6:7, part = 'header') %>% 
  #   merge_at(j = 1, i = 1:2, part = 'header') %>% 
  #   merge_at(j = 2, i = 1:2, part = 'header') %>% 
  #   merge_at(j = 5, i = 1:2, part = 'header') %>% 
  #   merge_at(j = 8, i = 1:2, part = 'header') %>% 
  #   theme_vanilla() %>% 
  #   font(part = 'all', fontname = 'Times') %>% 
  #   bold(part = 'header') %>% 
  #   set_table_properties(width = 1, layout = 'autofit') %>% 
  #   # autofit(part = 'header') %>% 
  #   # autofit(add_w = 0, add_h = 0, part = 'body') %>% 
  #   align(align = 'center', part = 'header') %>% 
  #   border_inner_h(border = officer::fp_border(width = 0))
  
  save_as_docx(table, path = pathout)
}

make_habitat_table <- function(habitatdat, scale = 1000000, pathout) {
  
  tabledat <- habitatdat %>%
    select(year = group, habitat, total, lcl, ucl) %>%
    pivot_longer(total:ucl) %>%
    mutate(group = case_when(habitat %in% c('wetlands', 'rice', 'corn', 
                                            'other', 'totalfree') ~ 'A',
                             habitat %in% c('br_fall', 'br_spring', 
                                            'whep_fall', 'whep_vardd', 
                                            'incentives') ~ 'B',
                             TRUE ~ 'Grand Total'),
           group = factor(group, levels = c('A', 'B', 'Grand Total')),
           habitat = factor(habitat,
                            levels = c('wetlands', 'rice', 'corn', 'other', 
                                       'totalfree', 'br_fall', 'br_spring', 
                                       'whep_fall', 'whep_vardd', 'incentives')),
           habitat = case_when(habitat == 'incentives' ~ 'Total', 
                               habitat == 'totalfree' ~ 'Total',
                               habitat == 'br_fall' ~ 'BirdReturns fall',
                               habitat == 'br_spring' ~ 'BirdReturns spring',
                               habitat == 'whep_fall' ~ 'WHEP fall flooding',
                               habitat == 'whep_vardd' ~ 'WHEP variable drawdown',
                               habitat == 'other' ~ 'Other crops',
                               TRUE ~ str_to_sentence(habitat)),
           name = factor(name, levels = c('total', 'lcl', 'ucl')),
           year = gsub('-', '', year),
           value = value/scale) %>%
    arrange(group, habitat, year, name) %>%
    unite(col = 'label', name, year) %>%
    pivot_wider(names_from = label, values_from = value)
  
  table <- flextable(tabledat, 
                     col_keys = c('group', 'habitat', 
                                  'total_201314', 'ci1',
                                  'total_201415', 'ci2', 
                                  'total_201516', 'ci3',
                                  'total_201617', 'ci4')) %>% 
    flextable::compose(j = 'ci1',
                       value = as_paragraph('(',
                                            sprintf("%.03f", lcl_201314),
                                            "\U2012",
                                            sprintf("%.03f", ucl_201314), ')')) %>%
    flextable::compose(j = 'ci2',
                       value = as_paragraph('(',
                                            sprintf("%.03f", lcl_201415),
                                            "\U2012",
                                            sprintf("%.03f", ucl_201415), ')')) %>%
    flextable::compose(j = 'ci3',
                       value = as_paragraph('(',
                                            sprintf("%.03f", lcl_201516),
                                            "\U2012",
                                            sprintf("%.03f", ucl_201516), ')')) %>%
    flextable::compose(j = 'ci4',
                       value = as_paragraph('(',
                                            sprintf("%.03f", lcl_201617),
                                            "\U2012",
                                            sprintf("%.03f", ucl_201617), ')')) %>%
    set_header_labels(group = '',
                      total_201314 = '2013-14', 
                      ci1 = '2013-14',
                      total_201415 = '2014-15',
                      ci2 = '2014-15',
                      total_201516 = '2015-16',
                      ci3 = '2015-16',
                      total_201617 = '2016-17',
                      ci4 = '2016-17') %>% 
    merge_at(i = 1, j = 3:4, part = 'header') %>% 
    merge_at(i = 1, j = 5:6, part = 'header') %>% 
    merge_at(i = 1, j = 7:8, part = 'header') %>% 
    merge_at(i = 1, j = 9:10, part = 'header') %>% 
    merge_at(j = 1, i = 1:5, part = 'body') %>% 
    merge_at(j = 1, i = 6:10, part = 'body') %>% 
    theme_vanilla() %>% 
    font(part = 'all', fontname = 'Times New Roman') %>% 
    fontsize(part = 'all', size = 12) %>% 
    bold(part = 'header') %>% 
    set_table_properties(width = 1, layout = 'autofit') %>% 
    align(align = 'center', part = 'header') %>% 
    align(align = 'left', part = 'body', j = c(4,6,9,11,13)) %>% 
    align(align = 'center', part = 'body', j = c(1, 2)) %>% 
    border_inner_h(border = officer::fp_border(width = 0))
  
  save_as_docx(table, path = pathout)
}

make_shortfall_table <- function(shortfalltotals, shortfallseason, 
                                 scale = 1000000000, pathout) {
  values <- bind_rows(shortfalltotals %>% mutate(season = 'total') %>% select(-habitat),
                      shortfallseason) %>% 
    select(population, season, group, incentives, total, lcl, ucl)
  
  diff <- values %>% select(-lcl, -ucl) %>% 
    pivot_wider(names_from = incentives, values_from = total) %>% 
    mutate(diff = with - without,
           perc = diff / without * 100) %>% 
    select(population, season, group, perc)
  
  tabledat <- left_join(values, diff, by = c('population', 'season', 'group')) %>% 
    mutate_at(vars(total:ucl), ~./scale) %>% 
    pivot_longer(total:perc) %>% 
    mutate(incentives = factor(incentives, levels = c('without', 'with')),
           population = factor(population, levels = c('baseline', 'objectives')),
           name = factor(name, levels = c('total', 'lcl', 'ucl', 'perc')),
           season = factor(season, levels = c('fall', 'spring', 'total'))) %>% 
    arrange(population, incentives, name) %>% 
    unite(col = 'label', population, incentives, name) %>% 
    pivot_wider(names_from = label, values_from = value) %>% 
    arrange(season, group)
  
  
  table <- flextable(tabledat, 
                     col_keys = c('season', 'group', 
                                  'baseline_without_total', 'baseline_without_ci', 
                                  'baseline_with_total', 'baseline_with_ci', 
                                  'baseline_with_perc', ' ',
                                  'objectives_without_total', 'objectives_without_ci', 
                                  'objectives_with_total', 'objectives_with_ci', 
                                  'objectives_with_perc')) %>% 
    set_formatter(baseline_without_total = function(x) sprintf("%.02f", x),
                  baseline_with_total = function(x) sprintf("%.02f", x),
                  objectives_without_total = function(x) sprintf("%.02f", x),
                  objectives_with_total = function(x) sprintf("%.02f", x),
                  baseline_with_perc = function(x) sprintf("%.0f%%", x),
                  objectives_with_perc = function(x) sprintf("%.0f%%", x)) %>% 
    flextable::compose(j = "baseline_without_ci",
                       value = as_paragraph('(', 
                                            sprintf("%.02f", baseline_without_lcl), 
                                            "\U2012", 
                                            sprintf("%.02f", baseline_without_ucl), ')')) %>%
    flextable::compose(j = "baseline_with_ci",
                       value = as_paragraph('(', 
                                            sprintf("%.02f", baseline_with_lcl), 
                                            "\U2012", 
                                            sprintf("%.02f", baseline_with_ucl), ')')) %>% 
    flextable::compose(j = "objectives_without_ci",
                       value = as_paragraph('(', 
                                            sprintf("%.02f", objectives_without_lcl), 
                                            "\U2012", 
                                            sprintf("%.02f", objectives_without_ucl), ')')) %>%
    flextable::compose(j = "objectives_with_ci",
                       value = as_paragraph('(', 
                                            sprintf("%.02f", objectives_with_lcl), 
                                            "\U2012", 
                                            sprintf("%.02f", objectives_with_ucl), ')')) %>% 
    set_header_labels(baseline_without_total = 'excluding incentive\nprograms', 
                      baseline_without_ci = 'excluding incentive\nprograms',
                      baseline_with_total = 'including incentive\nprograms',
                      baseline_with_ci = 'including incentive\nprograms',
                      baseline_with_perc = 'difference\n(%)',
                      objectives_without_total = 'excluding incentive\nprograms', 
                      objectives_without_ci = 'excluding incentive\nprograms',
                      objectives_with_total = 'including incentive\nprograms',
                      objectives_with_ci = 'including incentive\nprograms',
                      objectives_with_perc = 'difference\n(%)') %>% 
    merge_at(i = 1, j = 3:4, part = 'header') %>% 
    merge_at(i = 1, j = 5:6, part = 'header') %>% 
    merge_at(i = 1, j = 9:10, part = 'header') %>% 
    merge_at(i = 1, j = 11:12, part = 'header') %>% 
    add_header_row(values = c('season', 'group', 
                              'Baseline population', 'Baseline population', 
                              'Baseline population', 'Baseline population', 
                              'Baseline population', ' ',  
                              'Population objectives', 'Population objectives',
                              'Population objectives', 'Population objectives',
                              'Population objectives')) %>% 
    merge_at(i = 1, j = 3:7, part = 'header') %>% 
    merge_at(i = 1, j = 9:13, part = 'header') %>% 
    merge_at(j = 1, i = 1:2, part = 'header') %>% 
    merge_at(j = 2, i = 1:2, part = 'header') %>% 
    merge_at(j = 8, i = 1:2, part = 'header') %>% 
    theme_vanilla() %>% 
    font(part = 'all', fontname = 'Times New Roman') %>% 
    fontsize(part = 'all', size = 12) %>% 
    bold(part = 'header') %>% 
    set_table_properties(width = 1, layout = 'autofit') %>% 
    # autofit(part = 'header') %>% 
    # autofit(add_w = 0, add_h = 0, part = 'body') %>% 
    align(align = 'center', part = 'header') %>% 
    border_inner_h(border = officer::fp_border(width = 0))
  
  save_as_docx(table, path = pathout)
}

make_habitatneed_table <- function(filldat, scale, title, pathout) {
  
}



