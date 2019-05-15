Shorebird bioenergetics & Central Valley wetland incentives
-----------------------------------------------------------

This repo is for a study of the contribution of wetland incentives programs in the Central Valley (e.g. Bird Returns, WHEP) to shorebird bioenergetics, particularly during both dry and wet years during 2013-2017. The intention is to estimate the contribution of these wetlands to meeting CVJV habitat objectives and shrinking shorebird energy shortfalls during the non-breeding season.

### DATA CHECKLIST:

#### Habitat available & accessible

###### Bird Returns

-   DONE - Compile annual/seasonal incentive acreages from spatial data (possible further update from partners)
-   DONE - Overlay spatial data with original CVJV land cover data to check correspondence
-   DONE - Estimate proportion available and accessible by day of year during enrollment
    -   Assume 100% open water
    -   Apply Greg's estimates of general proportion accessible during enrollment by season
    -   Do not assume credit for any lingering flooding after enrollment that may occur
-   DONE - Estimate daily change in BR habitat available, accessible, added, and returned in each year

###### WHEP

-   DONE - Compile annual/seasonal incentive acreages by program type from Kristin/NRCS data downloads
-   DONE - Estimate proportion available by day of year during enrollment:
    -   Fall flooding: distribute area enrolled over period between 1 July and 15 Sept, assuming two weeks of flooding and two weeks of gradual drawdown
    -   Variable drawdown: apply Kristin's estimates of flood up dates (variable by year); distribute staggered drawdown over 4 weeks in Feb (with 1/4 of area starting to draw down during weeks 1, 2, 3, and 4), and assume it takes 2 weeks to go virtually dry
    -   Boards-in: not included to date
-   DRAFT - Estimate proportion accessible by day of year during enrollment:
    -   Fall flooding: assume 100% accessible because "shallow" flooding
    -   Variable drawdown: Apply original rice depth curves; 2012-13 depth data show fairly similar pattern (if anything original curves are generous and overestimate proportion suitable)

###### Wetlands & crops (potential habitat)

-   DONE - Compile annual statewide crop acreages from NASS through 2017, using original CVJV land cover data to estimate total acreage of each crop class within the CVJV primary focus area
-   DONE - Model proportion open water in each crop class through 2017 using water tracker data
    -   Adjust proportion open water by acreages enrolled in incentive programs
    -   Model proportion open water as a function of day of year
    -   Fit separate models for each year & compare to original CVJV flooding curves
    -   Predict proportion open water by day of year with confidence intervals for each crop class and year
    -   Use original flood curve for "other" crops (does not vary by year)
-   DONE - Estimate proportion accessible by day of year, assuming same proportion accessible as original CVJV analysis

###### Compilation across land cover types

-   DONE - Compile daily estimates of habitat available, accessible, added, and returned in all land cover types in all 4 years

#### Energy density & energy needs

-   DONE - Assume same energy densities by crop class as in original CVJV analysis
-   DONE - Assume same population objectives, baseline population size, and daily energy needs as original CVJV analysis

#### Run bioenergetics model

-   DRAFT - Run deterministic models for each year, with and without acreage from incentive programs
-   Re-run models with Monte Carlo simulations to estimate error/uncertainty

#### Summarize results

-   Annual variation in magnitude and timing of energy shortfalls
-   Differences in energy shortfalls with addition of incentive programs
-   Proportion of energy consumed in incentive acres
-   Total contribution of fall acres to spring shortfalls?
