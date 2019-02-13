fit_gamm4 = function(df, dayofyear = 'yday', nsampled = 'nsampled', 
                     nwater = 'nflooded', ntotal = 'ntotal', year = 'year',
                     minprop = NULL, plot = TRUE, k = 10, by = NULL) {
  require(gamm4)
  
  # set up variables:
  df <- df %>%
    rename(yday = dayofyear, ntotal = ntotal, nsampled = nsampled, 
           nwater = nwater, year = year) %>%
    mutate(zday = (yday - 150) / 100,
           nwater = as.integer(nwater),
           nsampled = as.integer(nsampled)) 

  # sampledat$precip.2wk = sampledat$precip.2wk/10 #convert tenths of mm to mm
  
  # set minimum proportion sampled for interval to be included in data set
  if (!is.null(minprop)) {
    df <- df %>% 
      mutate(weights = nsampled / ntotal) %>%
      filter(weights >= minprop)
  } else {
    df <- df %>%
      mutate(weights = NULL)
  }

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
    
    res <- gamm4(cbind(nwater, nsampled - nwater) ~ group + s(zday, k = k, by = group), 
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
  newdat$nwater <- NULL
  newdat$nsampled <- NULL
  newdat$prop.sampled <- NULL
  newdat$zday <- NULL

  # res <- list(mod = res, sampledat = df, pred = newdat)
  
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
  
  return(newdat)
    
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