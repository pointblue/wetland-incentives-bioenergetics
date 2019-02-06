# Master script

# README--------
rmarkdown::render(here::here("Rmd/README.Rmd"),
                  output_file = here::here("README.md"))


# 1. LAND COVER--------

# a) Annual acres planted in California from NASS Quickstats
# PACKAGES: rnass (rOpenSci interface to NASS quickstats)
# FUNCTIONS: extract_nass (custom function to extract the specific crop types 
#    we need for this project for a given year)
# OUTPUT: 
# - 'data/NASS_raw_statewide.csv' (raw data straight from NASS)
# - 'data/NASS_totals_statewide.csv' (total area by crop class and year)

source(here::here('code/1a_agstats.R'))

# b) Proportion of statewide crops that typically fall within the CVJV primary
#    focus area and each CVJV basin
# INPUTS:
# - 'data/annual_NASS_totals_statewide.csv' (from previous step)
# - 'data/ag_distribution_basins.csv' (typical proportion of statewide crop 
#      classes that fall in each basin - from prior CVJV work)
# OUTPUT: 
# - 'output/planted_crops_mean_2013-17.csv' (total area within CVJV by crop class and year)

source(here::here('code/1b_agtotals.R'))

# c) Annual and seasonal spatial extent of fields enrolled in Bird Returns
# PACKAGES: sf
# INPUTS:
# - confidential shapefile in 'data/CONFIDENTIAL_br_fieldsMaster'
# OUTPUTS:
# - 'data/BR_fields.csv' (anonymized table of when different fields were enrolled)
# - 'data/BR_timeseries.csv' (area of new or existing fields enrolled each day
#     of the shorebird nonbreeding season)
# - 'data/BR_enrollment_table.csv' (chart of which fields enrolled in each 
#     half-month of each yeear for use with creating seasonal land cover rasters)
# - 'data/BR_totals.csv' (seasonal summary of total acreage enrolled)

source(here::here('code/1c_BRstats.R'))

# d) Relationship between fields enrolled in Bird Returns and earlier CVJV
#    land cover data
# PACKAGES: raster, sf, fasterize
# INPUTS:
# - 'data/landcover/combined_landcover&cvjv.tif' (older CVJV land cover raster)
# - 'data/landcover/landcover_key.csv' (key to the coded raster values)
# - confidential shapefile in 'data/CONFIDENTIAL_br_fieldsMaster' 
# - 'data/BR_totals.csv' (from previous step)
# OUTPUTS:
# - 'output/BR_landcover.csv' (number and proportion of pixels from rasterized 
#    BR polygons that fall within each of the older CVJV land cover "zones")

source(here::here('code/1d_BRlandcover.R'))


