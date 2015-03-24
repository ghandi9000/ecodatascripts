Fits for different species/species groupings with different models/combinations of predictor variables.

## HH transects
Canopy heights: mean of tallest 5 trees
Species: ABBA (use for PIRU - only 2 PIRU), BECO
Model: Gompertz (canopy height), "./gompertz/can/"

## Permanent Plots
Canopy heights: local canopy - subplot when available, otherwise whole plot, estimated by mean of codominant and dominant trees

Species: ABBA
Years: 86, 98, 10
Model: Gompertz (canopy, elevation), "./gompertz/full/abba"

Species: PIRU
Years: 86, 87, 98, 10
Model: Gompertz(canopy, elevation), "./gompertz/full/piru"

Species: SOAM (use PRPE try 1986 fit together)
years: 98 (there is some data for 2010, but couldn't get good fit)
Model: Gompertz(canopy, elevation), "./gompertz/full/soam"

Species: ACSA, ACSP, ACPE (maples)
years: 98, 10
Model: Gompertz(canopy, elevation), "./gompertz/full/maples"

Species: FAGR, only 5 points, fit with maples (hardwoods)
Years: 98, 10
Model: Gompertz(canopy, elevation), "./gompertz/full/hardwoods"

Species: BECO
Years: 86, 98 (added additional BECO data from Matt), 10
Model: Negexp(canopy, elevation), "./negexp/full/beco"

Species: BEPA, BEAL, BECO (betula)
Years: 86, 98 (added additional BECO data from Matt), 10
Model: Negexp(canopy, elevation), "./negexp/full/beco"

## Transect Plots (Not HH)
No canopy heights

Same as permanent plots except models involve only elevation.
"./gompertz/elev/"
"./negexp/elev/"

## Problems
SOAM fits not working for full gompertz model -- maybe need to swith to negexp model.  They work fine with gompertz(elevation only).

## TODO
* Cleanup original fitting scripts for full model.
* Unify all fits into single wrapper script for refitting.
* Add option to save 3d scripts to visualize_fits (check under can/visualize_fits)
* Add ggplot2 visualization
* Add ability to print unified fit diagnostics (rerunning only quick version of fits from pars)
* Write script to apply all the fits to different datasets
