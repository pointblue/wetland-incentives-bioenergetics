##' @title Update flooding curves
##' @param path path to CSV containing cloud-filled water tracker data
##' @param metapath path to CSV containing additional meta data on pixels
##'   sampled prior to cloud-filling
##' @param ... tibble objects containing time series data on acres enrolled in
##'   incentive programs (to be subtracted from corresponding pixels of rice
##'   from water tracker)
##' @param dayofyear field name for the day of year (time interval) for which
##'   flood curves will be modeled
##' @param nsampled field name for the number of pixels of a crop class that
##'   were observed on an individual satellite image
##' @param nwater field name for the number of pixels of a crop class estimated
##'   to contain open water on an individual satellite image date
##' @param ntotal field name for the total number of pixels of a crop class that
##'   could be sampled on an individual satellite image
##' @param year field name for the individual year being modeled
##' @param weights field name for weights given to individual satellite images
##' @param plot logical; whether not a plot of the resulting flood curve should
##'   be produced
##' @param k number of knots in the GAMM (see gamm4); default 10
##' @param by default NULL; optional character/factor vector by which flooding
##'   curves should be fit separately
##' @param seas total estimated area of seasonal wetlands (hectares)
##' @param perm total estimated area of permanent/semi-permanent wetlands
##'   (hectares)
##' @description Functions to compile water tracker data and fit flooding curves

get_waterdat = function(path, metapath) {
  df <- read_csv(path, col_types = cols()) %>%
    # drop extra column
    select(-X12) %>% 
    mutate_at(vars(MosaicDateStart:MosaicDateEnd), as.Date, format = '%m/%d/%Y',
              vars(DataSource:Mosaic), as.factor) %>%
    # keep only the crop classes we are using:
    filter(ClassName %in% c('Corn', 'Rice', 'Field Crop', 'Grains', 'Row Crop', 
                            'Wetland')) %>%
    # drop years we aren't using
    filter(MosaicDateStart < '2018-01-01') %>% 
    # drop non-existent Tulare Rice, drop Suisun Basin, drop Corn in San Joaquin and Tulare
    filter(!(Name %in% c('Tulare Rice', 'San Joaquin Corn', 'Tulare Corn')) & 
               BasinName != 'Suisun')
  
  # estimated total pixels of each crop in each basin (from PercentObserved and ObservedArea)  
  totals <- estimate_totals(df)
  
  # estimate proportion of original image that was cloudfree
  metadat <- get_image_quality(metapath, totals)
  
  df %>%
    # combine row, field, and grains into "Other"
    mutate(ClassName = recode(ClassName,
                              'Field Crop' = 'other',
                              'Row Crop' = 'other',
                              'Grains' = 'other',
                              'Wetland' = 'wetlands'),
           ClassName = tolower(ClassName)) %>%
    # summarize cloud-filled open water data by crop class and basin in each mosaic
    group_by(ClassName, BasinName, Mosaic, MosaicDateStart, MosaicDateEnd) %>%
    summarize(nflooded = sum(ObservedAreaWater, na.rm = TRUE),
              nsampled = sum(ObservedArea), .groups = 'drop') %>%
    
    # add crop-specific totals:
    left_join(totals, by = c('BasinName', 'ClassName')) %>% 
    
    # summarize cloud-filled open water data by crop class over all basins in each mosaic
    #  (also convert to number of 30x30m pixels):
    group_by(ClassName, Mosaic, MosaicDateStart, MosaicDateEnd) %>%
    summarize(nflooded = sum(nflooded) / 900,
              nsampled = sum(nsampled) / 900,
              ntotal = sum(basincroptotal) / 900, #same as croptotal
              prop.sampled = nsampled / ntotal, 
              .groups = 'drop') %>% 
    
    correct_bias() %>% 
    
    # add meta-data for mosaic image quality
    mutate(Mosaic = gsub('_cf10yr.tif', '.tif', Mosaic)) %>% #match mosaic names
    left_join(metadat, by = c('Mosaic', 'ClassName')) %>%
    
    # add mid-point of mosaic date range, assign day of year and bioyear labels:
    mutate(MosaicDateMid = MosaicDateStart + (MosaicDateEnd - MosaicDateStart)/2,
           month = as.numeric(format(MosaicDateMid, '%m')),
           yday = as.numeric(format(MosaicDateMid, '%j')),
           yday = case_when(month <= 6 ~ yday + 184,
                            month >= 7 ~ yday - 181),
           bioyear = as.numeric(format(MosaicDateMid, '%Y')),
           bioyear = case_when(month <=6 ~ bioyear - 1,
                               TRUE ~ bioyear)) %>%
    
    filter(bioyear >= 2013 & bioyear <= 2016 & yday <= 319) %>%
    mutate(group = recode(bioyear, 
                          '2013' = '2013-14',
                          '2014' = '2014-15',
                          '2015' = '2015-16',
                          '2016' = '2016-17'),
           ClassName = factor(ClassName, 
                              levels = c('wetlands', 'rice', 'corn', 'other'))) %>% 
    select(habitat = ClassName, bioyear, group, yday, ntotal, nsampled, 
           nflooded, prop.sampled, prop.cloudfree)
}

estimate_totals <- function(df) {
  df %>% 
    #  - largest number of pixels observed for each crop class in each basin
    group_by(Name, BasinName, ClassName) %>%
    summarize(basincroptotal = max(ObservedArea),
              percent = max(PercentObserved, na.rm = T), .groups = 'drop') %>% 
    # - adjust upward for few that were never 100% observed:
    mutate(basincroptotal = basincroptotal / (percent/100),
           percent = percent / (percent/100)) %>%
    # - group row, field, and grains into "Other"
    mutate(ClassName = recode(ClassName,
                              'Field Crop' = 'other',
                              'Row Crop' = 'other',
                              'Grains' = 'other',
                              'Wetland' = 'wetlands'),
           ClassName = tolower(ClassName)) %>%
    group_by(BasinName, ClassName) %>%
    summarize(basincroptotal = sum(basincroptotal), .groups = 'drop') %>%
    group_by(ClassName) %>%
    mutate(croptotal = sum(basincroptotal),
           basinprop.croptotal = basincroptotal/croptotal) %>%
    ungroup()
}

get_image_quality <- function(metapath, totals) {
  # image quality: percent of each crop & basin actually visible before cloud-filling
  meta <- read_csv(metapath, col_types = cols()) %>%
    filter(!is.na(BasinName) & BasinName != 'Suisun') %>%
    mutate(PercentObserved = case_when(is.na(PercentObserved) & ObservedArea == 0 ~ 0,
                                       TRUE ~ PercentObserved))
  
  # for each crop class and mosaic, estimate cloud-free proportion of pixels, 
  #   from the cloud-free proportion in each basin, weighted by the 
  #   proportion of each crop class in each basin (prior to any cloud filling)
  map_df(unique(totals$ClassName), function(x) {
    if (x == 'corn') {
      tmp <- meta %>% filter(!(BasinName %in% c('San Joaquin', 'Tulare')))
    } else {tmp <- meta}
    tmp %>% 
      left_join(totals %>% filter(ClassName == x) %>% 
                  select(BasinName, basinprop.croptotal), by = 'BasinName') %>%
      group_by(Mosaic) %>%
      summarize(ClassName = x,
                prop.cloudfree = sum(basinprop.croptotal * PercentObserved, na.rm = TRUE)/100,
                .groups = 'drop')
  })
}

# bias correction from Reiter et al. paper
correct_bias <- function(df) {
  df %>% 
    mutate(nflooded = case_when(ClassName == 'wetlands' ~ nflooded * 1.11,
                                ClassName == 'rice' ~ nflooded * 0.96,
                                ClassName == 'corn' ~ nflooded * 0.95,
                                TRUE ~ nflooded))
}

subtract_incentives <- function(df, ...) {
  incentives <- bind_rows(...) %>% 
    select(group, yday, incentives = available) %>%
    group_by(group, yday) %>% 
    # convert to m2, then 30x30m pixels
    summarize(incentives = sum(incentives) * 10000 / 900, .groups = 'drop')
  
  left_join(df, incentives, by = c('group', 'yday')) %>%
    mutate(nflooded_free = case_when(habitat == 'rice' ~ nflooded - incentives,
                                     TRUE ~ nflooded))
}

fit_gamm4 = function(df, dayofyear = 'yday', nsampled = 'nsampled', 
                     nwater = 'nflooded', ntotal = 'ntotal', year = 'year',
                     weights = NULL, plot = TRUE, k = 10, by = NULL) {
  require(gamm4)
  
  # set up variables:
  df <- df %>%
    rename(yday = dayofyear, ntotal = ntotal, nsampled = nsampled, 
           nwater = nwater, year = year, weights = weights) %>%
    mutate(zday = (yday - 150) / 100,
           nwater = as.integer(nwater),
           nsampled = as.integer(nsampled)) 

  # sampledat$precip.2wk = sampledat$precip.2wk/10 #convert tenths of mm to mm
  
  # unique sample id
  df$id <- c(1:nrow(df)) 
  
  print(paste('Observations:', nrow(df)))
  
  if (is.null(by)) {
    res <- gamm4(cbind(nwater, nsampled - nwater) ~ s(zday, k = k), 
                 random = ~(1 | year) + (1 | id), 
                 family = binomial, data = df, weights = weights)
    
    newdat <- data.frame(yday = seq(1, 319, 1), 
                         nwater = 0, 
                         nsampled = 0, 
                         prop.sampled = 0)

  } else if (!is.null(by)) {
    df <- df %>%
      rename(group = by) %>%
      mutate(group = as.factor(group))
    
    res <- gamm4(cbind(nwater, nsampled - nwater) ~ -1 + group + s(zday, k = k, by = group), 
                 random = ~(1 | id), 
                 family = binomial, data = df, weights = weights)
    
    newdat <- expand.grid(yday = seq(1, 319, 1), 
                          group = levels(df$group)) %>%
      mutate(nwater = 0, nsampled = 0, prop.sampled = 0)
  }
  
  # predict proportion open water by day of year with CI
  newdat$zday = (newdat$yday - 150) / 100
  pred <- as.data.frame(predict(res$gam, newdat, type = 'link', se.fit = T))
  newdat$fit <- plogis(pred$fit)
  newdat$lcl <- plogis(pred$fit - 2 * pred$se.fit)
  newdat$ucl <- plogis(pred$fit + 2 * pred$se.fit)

  Xp = predict(res$gam, newdat, type = 'lpmatrix')
  br = rmvn(10000, coef(res$gam), res$gam$Vp)

  newdat$nwater <- NULL
  newdat$nsampled <- NULL
  newdat$prop.sampled <- NULL
  newdat$zday <- NULL
  
  if (plot == TRUE) {
    require(ggplot2)
    p <- ggplot(newdat, aes(x = yday, y = fit)) +
      scale_x_continuous(limits = c(1, 319), 
                         expand = c(0, 0), 
                         breaks = c(1, 32, 62, 93, 124, 154, 185, 216, 244, 
                                    275, 305), 
                         labels = c('Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 
                                    'Jan', 'Feb', 'Mar', 'Apr', 'May')) +
      scale_y_continuous(limits = c(0, 1), 
                         expand = c(0, 0),
                         breaks = seq(0, 1, 0.2)) +
      xlab(NULL) + ylab('Proportion open water') +
      scale_size(range = c(1, 4), guide = 'none') +
      theme_classic() + 
      theme(legend.position = c(1, 1), legend.justification = c(1, 1))
    
    if (!is.null(by)) {
      p <- p + 
        geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = group), alpha = 0.3) +
        geom_line(aes(color = group), size = 1) +
        geom_point(data = df, 
                   aes(y = nwater/nsampled, size = nsampled/ntotal, 
                       color = group), shape = 21) 
    } else {
      p <- p + 
        geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.5, fill = 'gray50') + 
        geom_line() +
        geom_point(data = df, shape = 21,
                   aes(y = nwater/nsampled, size = nsampled/ntotal))
    }
    
    print(p)      
  }
  
  return(list(mod = res, pred = newdat, Xp = Xp, br = br))
    
  # } 
  # else if (precip == TRUE) {
  #   res <- gamm4(cbind(n.flooded, n.sampled - n.flooded) ~ s(zday, k = k) + precip.2wk, 
  #                random = ~(1 | wateryear) + (1 | id), 
  #                family = binomial, data = sub, weights = weight)
  #   
  #   ## predicted proportion open water with zero precip in previous 2wks
  #   newdat <- data.frame(yday = seq(1, 319, 1), 
  #                        precip.2wk = 0, 
  #                        prop.flooded = 0, 
  #                        n.flooded = 0, 
  #                        n.sampled = 0, 
  #                        prop.sampled = 0)
  #   newdat$zday = (newdat$yday - 150) / 100
  #   newdat$prop.flooded <- predict(res$gam, newdat, type = 'response')
  #   pred <- as.data.frame(predict(res$gam, newdat, type = 'link', se.fit = T))
  #   newdat$lcl <- plogis(pred$fit - 2 * pred$se.fit)
  #   newdat$ucl <- plogis(pred$fit + 2 * pred$se.fit)
  #   
  #   ## predicted proportion open water with 10mm in previous 2 wks -- range observed is 0-12.27
  #   newdat2 <- data.frame(yday = seq(1, 319, 1), 
  #                         precip.2wk = 10, 
  #                         prop.flooded = 0, 
  #                         n.flooded = 0, 
  #                         n.sampled = 0, 
  #                         prop.sampled = 0)
  #   newdat2$zday <- (newdat2$yday - 150) / 100
  #   newdat2$prop.flooded <- predict(res$gam, newdat2, type = 'response')
  #   pred2 <- as.data.frame(predict(res$gam, newdat2, type = 'link', se.fit = T))
  #   newdat2$lcl <- plogis(pred2$fit - 2 * pred2$se.fit)
  #   newdat2$ucl <- plogis(pred2$fit + 2 * pred2$se.fit)
  #   
  #   if (plot == TRUE) {
  #     require(ggplot2)
  #     p = ggplot(newdat, aes(x=yday, y=prop.flooded, ymin=lcl, ymax=ucl)) + 
  #       geom_ribbon(fill='gray80') + geom_line() + 
  #       geom_line(data=newdat2, aes(x=yday, y=prop.flooded), linetype='dashed') +
  #       geom_ribbon(data=newdat2, aes(x=yday, y=prop.flooded), fill='gray90', alpha=0.5) +
  #       theme_classic() + xlab(NULL) + ylab('Proportion open water') + 
  #       geom_point(data=sub, aes(ymin=NULL, ymax=NULL, size=precip.2wk, color=precip.2wk)) + 
  #       scale_x_continuous(limits=c(1,319), expand=c(0,0), breaks=c(1,32,62,93,124,154,185,216,244,275,305), 
  #                          labels=c('Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May'))
  #     print(p)      
  #   }
  #   res <- list(mod = res, sampledat = sampledat, pred = newdat)
  #   return(res)
  # }
}

# estimate percent of flooded wetlands that are semi-permanent vs. seasonal

## per Craig Isola: 
##  - seasonal wetlands start getting water in early Aug, so assume 100% of 
##      any wetlands before early August is a semi-permanent wetland (assumed 
##      to be day 34 in original CVJV work)
##  - peak of dryness in semi-permanent wetlands is mid-August through 
##      mid-October (e.g. day 107), so assume extent of flooded semi-permanent 
##      wetlands remains constant from early Aug (before seasonal wetlands
##      have any water) 
##  - all semi-permanent wetlands are fully flooded by early November 
##      (e.g. day 124), so from early Nov through May (end of season) assume: 
##      proportion of open water in wetlands that is semi-perm = 
##        total perm / prop.flooded*total wetlands
# -> fill in gap between day 107 and day 124 during flood-up in semi-permanent
#    wetlands with a spline

estimate_proportion_perm <- function(df, seas = 67849.127, perm = 6986.170) {
  total = seas + perm
  df %>%
    group_by(group, habitat) %>%
    mutate(minperm = fit[yday == 34],
           prop.perm = case_when(habitat != 'wetlands' ~ NA_real_,
                                 yday <= 34 ~ 1,
                                 yday > 34 & yday <= 107 ~ 
                                   (minperm * total)/(fit * total),
                                 yday >= 124 ~ perm/(fit * total),
                                 yday > 107 & yday < 124 ~ NA_real_,
                                 TRUE ~ 0),
           spline = ifelse(habitat == 'wetlands', 
                           spline(x = yday, y = prop.perm, method = 'natural', 
                                  xout = c(1:319))$y, 
                           NA_real_),
           prop.perm = ifelse(yday > 107 & yday < 124, spline, prop.perm),
           prop.perm = ifelse(prop.perm > 1, 1, prop.perm),
           minperm = NULL,
           spline = NULL) %>%
    ungroup()
}

add_original_curves <- function(df, origpath, subset = 'other') {
  replace <- expand.grid(group = unique(df$group),
                         yday = c(1:319)) %>%
    left_join(read_csv(origpath, col_types = cols(prop.perm = 'd')) %>%
                filter(habitat == subset),
              by = 'yday') %>%
    arrange(group, yday)
  
  bind_rows(df, replace)
}