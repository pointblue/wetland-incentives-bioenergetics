READ ME
-------

This repo is for a study of the contribution of wetland incentives programs in the Central Valley (e.g. Bird Returns, WHEP) to shorebird bioenergetics, particularly during both dry and wet years during 2013-2017. The intention is to estimate the contribution of these wetlands to meeting CVJV habitat objectives and shrinking shorebird energy shortfalls during the non-breeding season.

### CURRENT STATUS:

Data compilation and processing

### CHECKLIST:

#### Base acreages

-   Get annual crop acreages from NASS through 2017
-   Use CVJV distribution of crops (from same TNC layer) to estimate proportion of statewide acreages in the primary focus area and in each CVJV basin
-   Extract annual/seasonal incentive acreages from spatial data (subtract from rice in the CV?)
-   Overlay spatial incentive data with crop layer to generate new rasters of landcover type (for each year/sesaon?)

#### Proportion open water

-   Get annual surface water data through 2017 (from Matt/Nathan)
-   For each date and landcover type (including incentive acres as a "type"), extract proportion open water & proportion masked by clouds
-   For each landcover type, model proportion open water as a function of day of year (consider effect of precipitation or some annual effect of water year type)
-   For each land cover type and year (2013-14 through 2016-17), predict proportion open water by day of year with confidence intervals

#### Proportion accessible to shorebirds

-   For rice and wetlands, assume same depth curves as in CVJV paper (for comparability)
-   For BR acres, assume 100% suitable
-   For WHEP acres, base on rice depth curve + variable drawdown study?
