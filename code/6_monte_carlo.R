# README---------------
# Re-run bioenergetics models with resampled parameters to estimate uncertainty
# in flood curves and depth curves (for non-incentive-flooded acres), and 
# energy content per flooded acre (for all); assuming perfect information for
# change in habitat available from incentive acres
#
# PACKAGES
library(tidyverse)
library(bioenergmod) # devtools::install_github("kdybala/bioenergmod")

# INPUTS
floodsamples <- 'output/flooding_curve_resamples.RData'
depthsamples <- 'data/cvjv_orig/resamples_depth.RData'
energy_density <- 'data/cvjv_orig/energy_content.csv'
annual_acres <- 'data/landcover_totals_cvjv.csv'
objectives <- 'data/cvjv_orig/daily_energy_requirement.csv'
br_ts <- 'data/BR_timeseries.csv'
whep_ts <- 'data/WHEP_timeseries.csv'

# FUNCTIONS
source(here::here('functions/bioenergmod_mc_custom.R'))

# OUTPUTS


memory.limit(size = 24000)

# HABITAT AVAILABLE----------------
# resample from GAMMs; result should be a named list with elements for each
#   landcover type; each list contains estimates for each time step (rows) and
#   simulation (columns)

load(floodsamples) # resamples from updated flooding curves
str(floodsim)
# named list:
# corn, other, rice, wetlands: matrix of 1276 (4 years x 319 days) x 10000 resamples


# HABITAT ACCESSIBLE---------------
# use original CVJV resamples (since we're not updating these models)

load(depthsamples)
str(depthsim)
# named list:
# rice, perm, seas: matrix of 319 days x 10000 resamples (same each year)
# corn, other: vector of 10000 resamples (same each day in each year)

# update to match names
depthsim$corn <- depthsim$corn_north
depthsim$corn_north <- NULL


# ENERGY DENSITY------------
# resample from log normal distribution based on standard deviation

energydens <- read_csv(here::here(energy_density), col_types = cols()) %>%
  filter(habitat != 'corn') %>%
  mutate(habitat = recode(habitat, corn_north = 'corn'),
         habitat = factor(habitat, levels = c('perm', 'seas', 'rice', 'corn', 
                                              'other', 'whep_vardd', 
                                              'whep_fall', 'br')))

# add missing values for br and whep equivalent to rice:
energydens <- energydens %>%
  complete(habitat, fill = list(value = energydens$value[energydens$habitat == 'rice'], 
                                lcl = energydens$lcl[energydens$habitat == 'rice'], 
                                ucl = energydens$ucl[energydens$habitat == 'rice']))

# resample
energysim <- energydens %>% 
  mutate(logvalue = log(value/1000),
         logsd = (log(value/1000) - log(lcl/1000)) / 2) %>% 
  split(.$habitat) %>%
  map(function(x) {exp(rnorm(10000, mean = x$logvalue, sd = x$logsd)) * 1000})
str(energysim)
# named list: 
# perm, seas, rice, corn, other, whep_vardd, whep_fall, br: 
#   each a vector of 10000 resamples of starting energy content


# BIOENERGMOD WITH MCMC-------------
# additional data needed: (does not vary)
base <- read_csv(here::here(annual_acres), col_types = cols()) %>% 
  filter(!(habitat %in% c('seas', 'perm')))

needs <- read_csv(here::here(objectives), col_types = cols())

incentives <- bind_rows(read_csv(here::here(br_ts), col_types = cols()), 
                        read_csv(here::here(whep_ts), col_types = cols()))

# run for each year & scenario & set of objectives, separately:
# (custom function to calculate habitat change from )

test <- bioenergmod_mc_custom(nsim = 2, energyneed = needs$DER.obj,
                              tothabitat = base %>% filter(year == 2013),
                              energysim = energysim, floodsim = floodsim,
                              accessiblesim = depthsim, wetsplit = TRUE,
                              mintime = 1, maxtime = 319, grp = '2013-14',
                              addon = incentives)

### energy shortfall----------
scale = 1000000
es <- test$energy %>% apply(MARGIN = c(1, 2), mean) %>% as.data.frame() 

ggplot(es, aes(time, shortfall/scale)) + geom_line() + ylim(0, 250) + 
  geom_line(aes(y = DER/scale), linetype = 'dashed')


### energy consumed----------
ec <- test$energy.consumed[, 2:9, ] %>% apply(MARGIN = c(1, 2), mean) %>%
  as.data.frame() %>%
  mutate(total = rowSums(.),
         yday = c(1:319),
         DER = needs$DER.obj,
         wetlands = perm + seas) %>%
  select(-perm, -seas) 

ggplot(ec, aes(yday, total/scale)) + geom_area() + geom_line(aes(y = DER/scale))

# proportion of all calories consumed in each habitat type
ec %>% select(-total, -DER) %>% 
  gather(corn:whep_vardd, wetlands, key = 'habitat', value = 'value') %>% 
  group_by(habitat) %>%
  summarize(value = sum(value)) %>%
  ggplot(aes(1, value/sum(value), fill = habitat)) + geom_col() +
  ylab('proportion') + xlab(NULL)

