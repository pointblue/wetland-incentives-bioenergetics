# Master script

# README--------
rmarkdown::render(here::here("Rmd/README.Rmd"),
                  output_file = here::here("README.md"))


# INCENTIVE PROGRAMS---------------

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
# Compile time series of habitat available and accessible from fields enrolled 
# in WHEP programs. Distribute availability of fields across possible date
# ranges of each program.
#
# INPUTS:
# - 'data/WHEP_summary.xlsx' (raw estimates of acres enrolled by year)
# - 'data/cvjv_orig/depth_curves.csv' (original CVJV depth curves)
# OUTPUT:
# - 'data/WHEP_timeseries.csv' (area of new or existing fields enrolled each
#     day of the shorebird nonbreeding season)
# - 'data/WHEP_totals.csv' (seasonal summary of total acreage enrolled)

source(here::here('code/2_WHEP_habitat.R'))


# 2b. WHEP_depth_modeling
# Re-examine Kristin's depth data from variable drawdown fields in 2012-13 to 
# estimate depth curves (though these data may not be representative) in 
# comparison to original CVJV estimates for rice. (Does not change estimates
# generated in previous script 2 - keep using original CVJV estimates.)
#
# PACKAGES: gamm4
# INPUTS:
# - 'data/cvjv_orig/compiled_depth_data.csv' (raw estimates of depth by date)
# - 'data/cvjv_orig/depth_curves.csv' (original CVJV depth curves)
# OUTPUT:
# - 'output/whep_depth_models.csv' (predicted values for several alternative models)

source(here::here('code/2b_WHEP_depth_modeling.R'))


# OTHER HABITAT----------------

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
# - 'data/cvjv_orig/flooding_curves.csv' #original CVJV flood curves
# OUTPUTS:
# - 'output/open_water_annual.csv' (year- and crop-specific estimates of 
#     proportion open water by day of year)
# - 'output/flooding_curve_models.RData' (additional outputs from GAMM model 
#     structures)
# - 'output/flooding_curve_resamples.RData' (simulated estimates of open water
#     for use with Monte Carlo simulations) --NOTE: large file not on GitHub
#     but easily recreated


source(here::here('code/3b_update_flooding_curves.R'))


# RUN MODEL--------------------

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
# - 'data/cvjv_orig/flooding_curves.csv' #original CVJV flood curves
# - 'data/cvjv_orig/depth_curves.csv' #original CVJV depth curves
# - 'data/BR_timeseries.csv' #time series of bird returns habitat added/returned
# - 'data/WHEP_timeseries.csv' #time series of whep habitat added/returned
# OUTPUTS:
# - 'output/habitat_change.RData'
# - 'output/habitat_daily_stats.csv'
# - 'output/habitat_peak_stats.csv'

source(here::here('code/4_total_habitat_available.R'))


# 5. bioenergmod
# Run bioenergetics models
#
# PACKAGES: bioenergmod
# INPUTS:
# - 'data/cvjv_orig/daily_energy_requirement.csv'
# - 'data/cvjv_orig/energy_content.csv'
# - 'output/habitat_available.csv'
# - 'output/habitat_accessible.csv'
# - 'output/habitat_added.csv'
# - 'output/habitat_returned.csv'
# - 'output/habitat_prop.accessible.csv'
# OUTPUTS: 
# - 'output/bioenergetics_results.RData'
# - 'output/bioenergetics_results_energy.csv'
# - 'output/bioenergetics_results_energy_accessible.csv'
# - 'output/bioenergetics_results_energy_consumed.csv'
# - 'output/bioenergetics_results_energy_lost.csv'
#

source(here::here('code/5_bioenergmod.R'))

# 6. monte carlo
# Re-run bioenergetics models with resampled parameters to estimate uncertainty
# in flood curves and depth curves (for non-incentive-flooded acres), and 
# energy content per flooded acre (for all); assuming perfect information for
# change in habitat available from incentive acres
# 
# PACKAGES: bioenergmod
# INPUTS: 
# - 'output/flooding_curve_resamples.RData'
# - 'data/cvjv_orig/resamples_depth.RData'
# - 'data/cvjv_orig/energy_content.csv'
# - 'data/landcover_totals_cvjv.csv'
# - 'data/cvjv_orig/daily_energy_requirement.csv'
# - 'data/BR_timeseries.csv'
# - 'data/WHEP_timeseries.csv'
# FUNCTIONS: bioenergmod_mc_custom.R
# OUTPUTS: TBD
#

source(here::here('code/6_monte_carlo.R'))

# 7. plots
# Produce publication and presentation-ready plots
#
# INPUTS
# - 'output/open_water_annual.csv'
# - 'output/habitat_change.RData'
# - 'output/habitat_peak_stats.csv'
# - 'output/bioenergetics_results_energy.csv'
# - 'output/bioenergetics_results_energy_accessible.csv'
# - 'output/bioenergetics_results_energy_consumed.csv'
# - 'output/bioenergetics_results_energy_lost.csv'
# OUTPUTS
# - 'figs/flood_curves.png'
# - 'figs/habitat_open.png'
# - 'figs/habitat_accessible.png'
# - 'figs/energy_accessible_by_year.png'
# - 'figs/energy_shortfall_by_year.png'
# - 'figs/energy_effect_by_year.png' # % reduction in shortfall from incentive programs

