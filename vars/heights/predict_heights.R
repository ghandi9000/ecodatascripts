### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights for trees from basal areas for Moosilauke
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Mon Mar  2 14:29:10 2015 (-0500)
##           By: Noah Peart
######################################################################
library(splines)
library(dplyr)
dat <- read.csv("~/work/data/moose/moose-wide.csv")

