################################################################################
##
##                   Recreate moose-long.csv from raw data
##
## - moose-long.csv in ~/work/data/moose/moose-long.csv
##
################################################################################

## First make moose-wide.csv
source("~/work/ecodatascripts/recreate/make-moose-wide.R")

## Then transform to long, dropping columns on the way
source("~/work/ecodatascripts/trans/make-long-moose.R")

