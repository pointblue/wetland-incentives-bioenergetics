Shorebird bioenergetics & Central Valley wetland incentives
-----------------------------------------------------------

This repo is for a study of the contribution of wetland incentives programs in the Central Valley (e.g. Bird Returns, WHEP) to shorebird bioenergetics, particularly during both dry and wet years during 2013-2017. The intention is to estimate the contribution of these wetlands to meeting CVJV habitat objectives and shrinking shorebird energy shortfalls during the non-breeding season.

### DATA CHECKLIST:

#### Habitat available & accessible

##### Bird Returns

-   IN PROGRESS - Compile annual/seasonal incentive acreages from spatial data (waiting on updated data from partners)
-   IN PROGRESS - Overlay spatial data with original CVJV land cover data to check correspondence (waiting on updated data from partners, but mostly rice as expected)
-   DONE - Estimate proportion available and accessible by day of year during enrollment
    -   Assume 100% open water and 100% accessible during enrollment
    -   Do not assume credit for any lingering flooding after enrollment that may occur
-   DRAFT - Estimate daily change in BR habitat available, accessible, added, and returned in each year

##### WHEP

-   IN PROGRESS - Compile annual/seasonal incentive acreages by program type from Kristin/NRCS data downloads (waiting on potential spatial data)
-   If spatial data become available, overlay spatial data with original CVJV land cover data to check correspondence (waiting on potential spatial data)
-   DRAFT - Estimate proportion available by day of year during enrollment:
    -   Fall flooding: distribute area enrolled over period between 1 July and 15 Sept, assuming two weeks of flooding and two weeks of gradual drawdown
    -   Variable drawdown: distribute area enrolled in over 2 weeks in Nov for flood up, and over 4 weeks in Feb for staggered drawdown (with 1/4 of area drawn down during 1, 2, 3, or 4 weeks)
    -   Boards-in: not included to date
    -   Do not assume credit for any lingering flooding after enrollment that may occur
-   DRAFT - Estimate proportion accessible by day of year during enrollment:
    -   Fall flooding: assume 100% accessible because "shallow" flooding?
    -   Variable drawdown: Apply original rice depth curves

##### Wetlands & crops (potential habitat)

-   DONE - Compile annual statewide crop acreages from NASS through 2017, using original CVJV land cover data to estimate total acreage of each crop class within the CVJV primary focus area
-   DRAFT - Model proportion open water in each crop class through 2017 using water tracker data
    -   Model proportion open water as a function of day of year
    -   Try fitting separate models for each year
    -   Consider adjusting proportion open water by acreages enrolled in incentive programs? (but probably a tiny %)
    -   Predict proportion open water by day of year with confidence intervals for each crop class and year
-   DONE - Estimate proportion accessible by day of year, assuming same proportion accessible as original CVJV analysis

##### Compilation across land cover types

-   DRAFT - Compile daily estimates of habitat available, accessible, added, and returned in all land cover types in all 4 years

#### Energy density & energy needs

-   Assume same energy densities by crop class as in original CVJV analysis
-   Assume same population objectives, baseline population size, and daily energy needs as original CVJV analysis

#### Run bioenergetics model

-   DRAFT - Run deterministic models for each year, with and without acreage from incentive programs
-   Re-run models with Monte Carlo simulations to estimate error/uncertainty
