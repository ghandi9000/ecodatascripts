################################################################################
##
##      Read/clean/add transformed variables to recreate moose-wide.csv
##
################################################################################

## Read in raw data (this is done in the clean script)
## source("~/work/ecodatascripts/read/read-moose.R")

## Clean (i.e. remove some columns, rename stuff, etc.)
source("~/work/ecodatascripts/clean/clean-moose.R")

################################################################################
##
##                         Add transformed variables
##
## - NOTE: should move growth/prior columns to separate transform script
##
################################################################################

## Add slope
source("~/work/ecodatascripts/vars/slope/add-slope-moose.R")
## rm(list = ls()[!ls() %in% c("dat")])

## Add centered x,y values
source("~/work/ecodatascripts/vars/xy-values/add-xy-moose.R")

## Add z-values
source("~/work/ecodatascripts/vars/z-values/add-zvalues-moose.R")

## Add crown dimension variables (crdepth and crarea)
## source("~/work/ecodatascripts/vars/crowns/add-crowns-moose.R")

