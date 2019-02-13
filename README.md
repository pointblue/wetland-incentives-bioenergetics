Shorebird bioenergetics & Central Valley wetland incentives
-----------------------------------------------------------

This repo is for a study of the contribution of wetland incentives programs in the Central Valley (e.g. Bird Returns, WHEP) to shorebird bioenergetics, particularly during both dry and wet years during 2013-2017. The intention is to estimate the contribution of these wetlands to meeting CVJV habitat objectives and shrinking shorebird energy shortfalls during the non-breeding season.

### DATA CHECKLIST:

#### Habitat available & accessible

##### Bird Returns

-   IN PROGRESS - Compile annual/seasonal incentive acreages from spatial data (waiting on updated data from partners)
-   IN PROGRESS - Overlay spatial data with original CVJV land cover data to check correspondence (waiting on updated data from partners, but mostly rice as expected)
-   Assume 100% available during enrollment (100% open water)
-   Assume 100% accessible during enrollment (100% suitable depth for shorebirds)
-   Do not assume credit for any lingering flooding after enrollment that may occur
-   Do we care about precipitation?

##### WHEP

-   IN PROGRESS - Compile annual/seasonal incentive acreages by program type from Kristin/NRCS data downloads (waiting on potential spatial data)
-   If spatial data become available, overlay spatial data with original CVJV land cover data to check correspondence (waiting on potential spatial data)
-   Assume 100% available habitat during enrollment? (boards-in only floods with rain?)
-   Do not assume 100% accessible during enrollment? Base on rice proportion accessible plus adjustments from variable drawdown study?
-   Do we care about precipitation?

##### Wetlands & crops (potential habitat)

-   DONE - Compile annual statewide crop acreages from NASS through 2017, using original CVJV land cover data to estimate total acreage of each crop class within the CVJV primary focus area
-   IN PROGRESS - Model proportion open water in each crop class through 2017 using water tracker data
    -   Model proportion open water as a function of day of year
    -   Consider fitting separate models for each year, or effect of precipitation/annual water year type? (2013-14 = critical; 2014-15 = critical; 2015-16 = dry/below normal; 2016-17 = wet)
    -   Consider adjusting proportion open water by acreages enrolled in incentive programs? (but probably a tiny %)
    -   Predict proportion open water by day of year with confidence intervals for each crop class and year
-   Assume same proportion accessible as original CVJV analysis

#### Energy density & energy needs

-   Assume same energy densities by crop class as in original CVJV analysis
-   Assume same population objectives, baseline population size, and daily energy needs as original CVJV analysis
