## library() calls go here
library(conflicted)
library(dotenv)
# library(drake)
library(tidyverse)
library(sf)
library(readxl)
library(rnass) #remotes::install_github('emraher/rnass')
library(gamm4)
library(bioenergmod) #remotes::install_github('kdybala/bioenergmod')
library(patchwork)
library(flextable)
library(officer)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("gather", "tidyr")
conflicted::conflict_prefer("read_xlsx", "readxl")

# API for rnass package:
# acquire from: https://quickstats.nass.usda.gov/api
# --> assume stored as NASS_TOKEN in .Renviron
# usethis::edit_r_environ()
