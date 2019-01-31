# Master script

# README--------
rmarkdown::render(here::here("Rmd/README.Rmd"),
                  output_file = here::here("README.md"))

# BASE ACRES--------
# Annual acres planted in California from NASS Quickstats
# Note: currently not working because Quickstats server appears to be down
source(here::here('code/02_agstats.R'))

