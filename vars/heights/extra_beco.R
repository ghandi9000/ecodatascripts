### extra_beco.R --- 
## Filename: extra_beco.R
## Description: Add the extra BECO data for fits
## Author: Noah Peart
## Created: Tue Mar 17 20:06:51 2015 (-0400)
## Last-Updated: Tue Mar 17 23:05:33 2015 (-0400)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/prep.R")
source("~/work/ecodatascripts/read/read-birch.R")
library(dplyr)
library(ggplot2)

## Use 98 data
becos98  <- becos %>% select(ends_with("98"), ELEVCL) %>%
    filter(STAT98 == "ALIVE", !is.na(HT98), !is.na(DBH98)) %>%
        mutate(HT = HT98/100, DBH98 = DBH98/10) %>%
            select(HT, DBH=DBH98, ELEVCL)
becos98$ELEV <- ifelse(becos98$ELEVCL == "M", 990, 1065)

## Compare to other 98 data
compare_beco_data <- function() {
    dat98 <- prep_data(pp, 98, "beco")
    dat98 <- dat98 %>% select(HT=HTTCR98, DBH = DBH98, ELEVCL) %>% mutate(TYPE=0)
    becos98 <- becos98 %>% mutate(TYPE=1)
    both <- rbind(dat98, becos98)
    both$TYPE <- factor(both$TYPE, labels = c("PP", "Matt"))
    ggplot(both, aes(DBH, HT, col = TYPE)) + geom_point() + facet_grid(ELEVCL ~ .) + 
        ggtitle(label = "BECO data from 1998")
}

join_extra_beco <- function(dat, becos98) {
    dat98 <- prep_data(pp, 98, "beco")
    dat98 <- dat98 %>% select(HT=HTTCR98, DBH = DBH98, ELEVCL, ELEV) %>% mutate(TYPE=0)
    becos98 <- becos98 %>% mutate(TYPE=1)
    both <- rbind(dat98, becos98)
    return( both )
}
