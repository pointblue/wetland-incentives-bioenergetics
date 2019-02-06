# README--------
# Overlay land cover raster from prior CVJV work (including wetlands and 
#   consistent crop classes within the CVJV primary focus area) with fields 
#   included in incentive programs -- treat those fields as a different land 
#   cover type with its own flooding schedule

# PACKAGES
library(raster)
library(sf)
library(fasterize)
library(tidyverse)

# INPUT DATA
cvjv_landcover <- 'data/GIS/landcover/combined_landcover&cvjv.tif'
zonekey <- 'data/GIS/landcover/landcover_key.csv'
brfields <- 'data/GIS/CONFIDENTIAL_br_fieldsMaster'
br_totals <- 'data/BR_totals.csv'

# OUTPUT DATA
br_landcover <- 'data/BR_landcover.csv'


# CVJV LAND COVER DATA----------
cvjv <- raster(here::here(cvjv_landcover))
show(cvjv)
# rasterize incentive program polygons to match

# INCENTIVE PROGRAM POLYGONS------
shp <- st_read(here::here(brfields), 'CONFIDENTIAL_br_fieldsMaster') %>%
  mutate(polygonID = c(1:nrow(.))) %>%
  select(polygonID) %>%
  st_transform(crs = proj4string(cvjv))


brdat <- read_csv(here::here(br_totals)) %>%
  gather(jan_a:oct_b, key = 'opt', value = 'value') %>%
  filter(value == 1) 

memory.limit(size = 24000)
brdat_raster <- inner_join(shp, 
                           brdat %>% select(polygonID, GIS_Ac, value) %>% distinct(), 
                           by = 'polygonID') %>%
  fasterize(cvjv, field = "value")

# overlay br fields with cvjv land cover to determine how many pixels of br fields
#   are showing up in each crop class (zone)
stats <- zonal(brdat_raster, cvjv, fun = 'sum', digits = 0, na.rm = TRUE)
stats2 <- stats %>% as.tibble(.) %>%
  mutate(prop = round(sum / sum(sum), digits = 2)) %>%
  full_join(read_csv(here::here(zonekey)), by = 'zone')
write_csv(stats2, here::here(br_landcover))
# 88% rice, with a smattering of pixels in other land cover classes



# # HOLD: unclear if this is actually necessary...
# 
# # create rasters for each year and half-month, giving a value of 500 to each pixel
# # enrolled in the program
# shp2014 <- inner_join(shp, brdat %>% filter(year == 2014), by = 'polygonID') %>%
#   fasterize(cvjv, field = "value", by = "opt")
# save("shp2014", file = "data/landcover/br2014.RData")

