Shorebird bioenergetics & Central Valley wetland incentives
-----------------------------------------------------------

This repo is for a study of the contribution of wetland incentives programs in the Central Valley (e.g. Bird Returns, WHEP) to shorebird bioenergetics, particularly during both dry and wet years during 2013-2017. The intention is to estimate the contribution of these wetlands to meeting CVJV habitat objectives and shrinking shorebird energy shortfalls during the non-breeding season.

### CURRENT STATUS:

Data compilation and processing

### CHECKLIST:

#### Land cover (potential habitat)

-   DONE - Get annual crop acreages from NASS through 2017
-   DONE - Use CVJV distribution of crops (from same TNC layer) to estimate proportion of statewide acreages in the primary focus area and in each CVJV basin
-   IN PROGRESS - Extract annual/seasonal incentive acreages from spatial data (waiting on data from partners)
-   IN PROGRESS - Overlay polygons from incentive programs with old CVJV land cover data to check correspondence (complete for Bird Returns?)
-   TBD: Necessary? - Generate new land cover rasters including fields enrolled in incentive programs as their own land cover type (but separate rasters for each year/month?)

#### Flooding curves

-   Proportion open water: repeat prior CVJV process to extract proportion open water by crop class and date of satellite imagery through 2017
-   Get access to recent surface water data (from Matt/Nathan/water tracker)
-   For each date and landcover type (including incentive acres as a "type"), extract proportion open water & proportion masked by clouds (or use cloud-filled imagery?)
-   Alternative option: use data already available on water tracker time series?
-   For fields enrolled in incentive programs, assume 100% open water?

-   Flooding curve models:
-   For rice, adjust proportion open water on dates during the incentive programs by subtracting the proportion of all rice enrolled in incentive programs that should be flooded during that time of year (probably a tiny %?)
-   For each landcover type, model proportion open water as a function of day of year (consider effect of precipitation or some annual effect of water year type?)
-   For each land cover type and year (2013-14 through 2016-17), predict proportion open water by day of year with confidence intervals
-   For fields enrolled in incentive programs, assume 100% flooded on day 1? Generate comparable time series for each year with the proportion open water of all fields enrolled in each incentive program by day of year

#### Proportion accessible to shorebirds

-   For rice and wetlands, assume same depth curves as in CVJV paper (for comparability)
-   For BR acres, assume 100% suitable
-   For WHEP acres, base on rice depth curve + variable drawdown study?
