# README--------------
# Re-examine Kristin's depth data from variable drawdown fields to estimate
# depth curves (though these data may not be representative) in comparison to 
# original CVJV estimates for rice.
#
# Approach (same as CVJV): turn depth into suitable (1) or not (0) and run a 
# mixed-effects logistic regression with random effects specified as the survey 
# area ('sitecode') because repeated measurements.

# PACKAGES:
library(tidyverse)
library(gamm4)


# INPUT DATA
rawdat <- 'data/cvjv_orig/compiled_depth_data.csv'
depthcurves <- 'data/cvjv_orig/depth_curves.csv'

# OUTPUT DATA
whep_depth <- 'output/whep_depth_models.csv'

# SET UP DATA FOR ANALYSIS
dat <- read_csv(here::here(rawdat), col_types = cols()) %>%
  # filter(trt != 'Drill') %>%
  filter(sourcefile == 'Variable_drawdown_visit_data.xls') %>%
  mutate(depth.cm = avg.depth.in * 2.54,
         suitable = case_when(perc.flood == 0 & depth.cm == 0 ~ NA_real_,
                              depth.cm < 10 ~ 1,
                              depth.cm > 10 ~ 0),
         year = as.numeric(format(date.surv, '%Y')),
         mo = as.numeric(format(date.surv, '%m')),
         water.year = case_when(mo <= 6 ~ year - 1,
                                TRUE ~ year),
         yday = as.numeric(format(date.surv, '%j')),
         yday = case_when(mo <= 6 ~ yday + 184,
                          mo >= 7 ~ yday - 181),
         zday = (yday - 150)/100,
         trt = as.factor(trt)) %>%
  unite('sitecode', property, sitecode, remove = FALSE) %>%
  arrange(property, sitecode, date.surv) %>%
  mutate(sitecode = as.factor(sitecode),
         property = as.factor(property)) %>%
  select(property, sitecode, date.surv, trt, suitable, yday, zday) %>%
  drop_na(suitable)

summary(dat$trt)
summary(dat$yday)
str(dat)

# GAMM---------------
# logistic regression on day of year by treatment (delay in drawdown)

(modg <- gamm4(suitable ~ s(zday, k = 4, by = trt), dat, 
               random = ~(1|sitecode) + (1|property), 
               family = binomial))

# compare overall average gamm? add trt random effect to balance?
(modg2 <- gamm4(suitable ~ s(zday, k = 4), dat, 
                random = ~(1|sitecode) + (1|property) + (1|trt), 
                family = binomial))

# try eliminating week 1 (included in original cvjv depth curves and equivalent
#  to standard practice)
(modg3 <- gamm4(suitable ~ s(zday, k = 4), dat %>% filter(trt != 'WEEK1'), 
                random = ~(1|sitecode) + (1|property) + (1|trt), 
                family = binomial))

## predicted values
newdat <- expand.grid(trt = levels(dat$trt),
                      yday = seq(214, 319, 1)) %>%
  mutate(zday = (yday - 150)/100)

pred <- predict(modg$gam, newdat, type = 'link', se.fit = T) %>%
  as.data.frame()

pred2 <- predict(modg2$gam, newdat, type = 'link', se.fit = T) %>%
  as.data.frame() %>%
  rename(fit2 = fit, se.fit2 = se.fit)

pred3 <- predict(modg3$gam, newdat, type = 'link', se.fit = T) %>%
  as.data.frame() %>%
  rename(fit3 = fit, se.fit3 = se.fit)

newdat <- newdat %>%
  bind_cols(pred) %>%
  mutate(pred = plogis(fit),
         lcl = plogis(fit - 2 * se.fit),
         ucl = plogis(fit + 2 * se.fit)) %>%
  bind_cols(pred2) %>%
  mutate(pred2 = plogis(fit2),
         lcl2 = plogis(fit2 - 2 * se.fit2),
         ucl2 = plogis(fit2 + 2 * se.fit2)) %>%
  bind_cols(pred3) %>%
  mutate(pred3 = plogis(fit3),
         lcl3 = plogis(fit3 - 2 * se.fit3),
         ucl3 = plogis(fit3 + 2 * se.fit3))


write_csv(newdat, here::here(whep_depth))

# PLOT----------------
# summarize observed data for plotting:
tmp <- dat %>% 
  group_by(trt, yday, zday, suitable) %>%
  count()


ggplot(newdat, aes(x = yday, y = pred, group = trt)) + 
  # geom_ribbon(data = newdat %>% filter(trt == 'WEEK1'),
  #             aes(ymin = lcl2, ymax = ucl2), fill = 'gray50') +
  # geom_line(data = newdat %>% filter(trt == 'WEEK1'), aes(y = pred2)) +
  geom_ribbon(data = newdat %>% filter(trt == 'WEEK1'),
              aes(ymin = lcl3, ymax = ucl3), fill = 'gray50') +
  geom_line(data = newdat %>% filter(trt == 'WEEK1'), aes(y = pred3), linetype = 'dashed') +
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = trt), alpha = 0.5) + 
  geom_line(aes(color = trt)) +
  ylim(0, 1) + xlab(NULL) + ylab('Proportion suitable depth') +
  scale_x_continuous(limits = c(214, 258), 
                     breaks = c(216, 244), 
                     labels = c('Feb', 'Mar')) +
  geom_jitter(data = tmp, aes(y = suitable, size = n, color = trt), 
              alpha = 0.5, width = 0, height = 0.05)


# compare original rice depth curves: these data are much steeper
orig <- read_csv(here::here(depthcurves)) %>% filter(habitat == 'rice')

ggplot(newdat, aes(x = yday)) +
  geom_ribbon(data = orig, aes(ymin = lcl, ymax = ucl), fill = 'gray50') +
  geom_line(data = orig, aes(y = fit), size = 1) +
  # geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = trt), alpha = 0.5) + 
  # geom_line(aes(y = pred, color = trt)) +
  geom_ribbon(data = newdat %>% filter(trt == 'WEEK1'),
              aes(ymin = lcl2, ymax = ucl2), alpha = 0.6) +
  geom_line(data = newdat %>% filter(trt == 'WEEK1'), aes(y = pred2)) +
  geom_ribbon(data = newdat %>% filter(trt == 'WEEK1'),
              aes(ymin = lcl3, ymax = ucl3), alpha = 0.6) +
  geom_line(data = newdat %>% filter(trt == 'WEEK1'), aes(y = pred3), linetype = 'dashed') +
  ylim(0, 1) + xlab(NULL) + ylab('Proportion suitable depth') +
  scale_x_continuous(limits = c(185, 275),
                     breaks = c(185, 216, 244, 275), 
                     labels = c('Jan', 'Feb', 'Mar', 'Apr'),
                     expand = c(0, 0)) +
  geom_jitter(data = tmp, aes(y = suitable, size = n, color = trt), 
              alpha = 0.5, width = 0, height = 0.05)

## CONCLUSION: 
## in comparison to original cvjv depth curves estimated for rice, depth curves 
## in variable drawdown fields appear to start at a lower proportion suitable, 
## and more rapidly drop to 100% suitable, arriving before the "average" rice field
##  --> Kristin indicated these data may not be "representative" of variable
##      drawdown fields, but for now, it seems reasonable to apply original
##      depth curves to these fields
## In general, these curves are likely to be overly generous

