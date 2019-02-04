# Master script

# README--------
rmarkdown::render(here::here("Rmd/README.Rmd"),
                  output_file = here::here("README.md"))


# LAND COVER--------

# 1. Annual acres planted in California from NASS Quickstats
# PACKAGES: rnass (rOpenSci interface to NASS quickstats)
# FUNCTIONS: extract_nass (custom function to extract the specific crop types 
#    we need for this project for a given year)
# OUTPUT: 
# - 'data/annual_NASS_raw_statewide.csv' (raw data straight from NASS)
# - 'data/annual_NASS_totals_statewide.csv' (total area by crop class and year)

source(here::here('code/1a_agstats.R'))

# 2. Proportion of statewide crops that typically fall within the CVJV primary
#    focus area and each CVJV basin
# INPUTS:
# - 'data/annual_NASS_totals_statewide.csv' (from previous step)
# - 'data/ag_distribution_basins.csv' (typical proportion of statewide crop 
#      classes that fall in each basin - from prior CVJV work)
# OUTPUT: 
# - 'output/planted_crops_mean_2013-17.csv' (total area within CVJV by crop class and year)

source(here::here('code/1b_agtotals.R'))
