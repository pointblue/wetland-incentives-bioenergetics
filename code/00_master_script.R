# Master script

# README--------
rmarkdown::render(here::here("Rmd/README.Rmd"),
                  output_file = here::here("README.md"))


# HABITAT AVAILABILITY & ACCESSIBILITY--------

# 1. BR habitat
# Compile time series of habitat available and accessible from fields enrolled 
# in Bird Returns program. Assume fields are 100% available (open water) and
# accessible (suitable shorebird depth) during enrollment.
#
# PACKAGES: sf
# INPUTS:
# - confidential shapefile in 'data/GIS/CONFIDENTIAL_br_fieldsMaster'
# OUTPUTS:
# - 'data/BR_fields.csv' (anonymized table of when different fields were enrolled)
# - 'data/BR_timeseries.csv' (area of new or existing fields enrolled each day
#     of the shorebird nonbreeding season)
# - 'data/BR_enrollment_table.csv' (chart of which fields enrolled in each 
#     half-month of each yeear for use with creating seasonal land cover rasters)
# - 'data/BR_totals.csv' (seasonal summary of total acreage enrolled)

source(here::here('code/1_BR_habitat.R'))


# 1b. BR spatial
# Overlay BR fields with original CVJV land cover raster to check correspondence
#
# PACKAGES: raster, sf, fasterize
# INPUTS:
# - 'data/GIS/landcover/combined_landcover&cvjv.tif' (older CVJV land cover raster)
# - 'data/GIS/landcover/landcover_key.csv' (key to the coded raster values)
# - confidential shapefile in 'data/GIS/CONFIDENTIAL_br_fieldsMaster' 
# - 'data/BR_totals.csv' (from previous step)
# OUTPUTS:
# - 'data/BR_landcover.csv' (number and proportion of pixels from rasterized 
#    BR polygons that fall within each of the older CVJV land cover "zones")

source(here::here('code/1b_BR_spatial.R'))


# 2. WHEP
# Compile time series of habitat available from fields enrolled 
# in WHEP programs. Distribute availability of fields across possible date
# ranges of each program.
# Still to do: add in estimates of habitat accessible (based on rice depth curves)
#
# INPUTS:
# - 'data/WHEP_summary.xlsx' (raw estimates of acres enrolled by year)
# OUTPUT:
# - 'data/WHEP_timeseries.csv' (area of new or existing fields enrolled each
#     day of the shorebird nonbreeding season)
# - 'data/WHEP_totals.csv' (seasonal summary of total acreage enrolled)

source(here::here('code/2_WHEP_habitat.R'))


# 3. Crops and wetland habitat
# STEP 1: update area of potential habitat in each year through 2017
#
# PACKAGES: rnass (rOpenSci interface to NASS quickstats)
# FUNCTIONS: extract_nass (custom function to extract the specific crop types 
#    we need for this project for a given year)
# INPUTS:
# - 'data/cvjv_orig/ag_distribution_basins.csv' (typical proportion of statewide crop 
#      classes that fall in each basin - from prior CVJV work)
# OUTPUTS: 
# - 'data/NASS_raw_statewide.csv' (raw data straight from NASS)
# - 'data/NASS_totals_statewide.csv' (total area by crop class and year)
# - 'data/NASS_totals_cvjv.csv' (total area within CVJV by crop class and year)

source(here::here('code/3_crops_and_wetlands.R'))


# 3b. Update flooding curves
# STEP 2: update flooding curves for crops and wetland habitats to reflect
#  recent drought and wet years
#
# PACKAGES: gamm4
# FUNCTIONS: fit_flooding_curves_gamm4.R (custom function to fit GAMMs to water 
#   tracker data of proportion open water by date)
# INPUTS:
# - 'data/stats_basin_ag.csv' #data from water tracker (via Nathan)
# - 'data/BR_timeseries.csv' (from step 1 above)
# - 'data/WHEP_timeseries.csv' (from step 2 above)
# OUTPUTS:
# - 'output/open_water_annual.csv' (year- and crop-specific estimates of 
#     proportion open water by day of year)
# - 'output/open_water_by_wateryear.csv' (water-year and crop-specific
#     estimates of proportion open water by day of year -- 13-14 and 14-15 are
#     grouped here)
# - 'output/open_water_overall.csv' (crop-specific estimates of proportion open
#     water by day of year, for all years combined)

source(here::here('code/3b_update_flooding_curves.R'))


# 4. Total habitat available
# Estimate total open water and accessible open water in all land cover types 
# on each day of the nonbreeding season from updated flooding curves and 
# original CVJV depth curves (i.e., proportion of open water in each land cover 
# type that is of suitable depth for shorebird foraging habitat), plus data 
# from Bird Returns and WHEP programs
#
# PACKAGES: bioenergmod (devtools::install_github("kdybala/bioenergmod"))
# INPUTS:
# - 'data/NASS_totals_cvjv.csv' #year-specific extent of each crop class
# - 'output/open_water_annual.csv' #year-specific flooding curve estimates
# - 'data/cvjv_orig/depth_curves.csv' #original CVJV depth curves
# - 'data/BR_timeseries.csv' #time series of bird returns habitat added/returned
# - 'data/WHEP_timeseries.csv' #time series of whep habitat added/returned
# OUTPUTS:
# - 'output/habitat_available.csv'
# - 'output/habitat_accessible.csv'
# - 'output/habitat_added.csv'
# - 'output/habitat_returned.csv'
# - 'output/habitat_prop.accessible.csv'

source(here::here('code/4_total_habitat_available.R'))

