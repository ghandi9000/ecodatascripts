### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights for trees from basal areas for Moosilauke
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Wed Mar  4 21:28:27 2015 (-0500)
##           By: Noah Peart
######################################################################
library(splines)
library(dplyr)
source("~/work/ecodatascripts/vars/heights/canopy.R")

## Returns estimated canopy height and dbh of canopy trees
## (simply the average of trees with certain crown position)
## NOTE: 1. By default uses crown positions from 88 for 86 trees, ("cposyrs" variable)
##       2. For plot 15, heights were measured in 87 (modify "htyrs" for this)
canopy_dims <- function(dat, plot=NULL, yrs=c(86, 87, 98, 10), htyrs = yrs, cposyrs=c(88, 87, 98, 10),
                        cols = list(plot="PPLOT", stat="STAT", ht="HTTCR", dbh="DBH", cpos="CPOS"),
                        canpos = c("c", "d")) {
    if (!missing(plot)) dat <- dat[dat[[cols$plot]] == plot, ]
    if (unique(dat[[cols$plot]]) == 15) htyrs[1] <- 87  # plot 15 hts measured in 87 for 86 dbhs
    res <- sapply(1:length(yrs), FUN = function(i) {
        inds <- (dat[[paste0(cols$stat, yrs[i])]] == "ALIVE") &
            (!is.na(dat[[paste0(cols$ht, htyrs[i])]])) &
                (dat[[paste0(cols$cpos, cposyrs[i])]] %in% canpos)
        c(ht = mean(dat[inds,][[paste0(cols$ht, htyrs[i])]]),   # canopy height
          dbh = mean(dat[inds,][[paste0(cols$dbh, yrs[i])]]))   # canopy dbh
    })
    data.frame(t(res), time = yrs)
}

## 2 degree, one knot b-splines
dat <- pp %>% filter(PPLOT > 3)

pp$PPLOT <- factor(pp$PPLOT)
fit <- lm(HTTCR86 ~ bs(DBH86, degree = 2) + PPLOT + SPEC, data = pp)

## Visualize
## with(can_dims, stripchart(time~ht, col = PPLOT, jitter=1))
plot <- 4
p4 <- pp %>% filter(PPLOT %in% plot, STAT10 == "ALIVE") %>% select(PPLOT, SPLOT, ASPCL, ends_with("86"))
p4can <- can_dims %>% filter(PPLOT %in% plot, time == 10)
## p4$ind <- ifelse(p4$ASPCL == "E", 1, 2)

with(p4, plot(DBH86, HTTCR86, pch = 15, col = "green"))
points(p4can$dbh, p4can$ht, col = "blue", pch = 16)

p4 %>% filter(!is.na(HTTCR10)) %>% distinct(SPLOT) %>% print
    
## Models
basis <- bs(p4$DBH86, knots=c(p4can$dbh))
summary(fit <- lm(HTTCR86 ~ bs(DBH86, knots=c(p4can$dbh)), data = p4))
summary(fit1 <- lm(HTTCR86 ~ DBH86, data = p4))
summary(fit2 <- nls(HTTCR86 ~ a*DBH86**b, data = p4, start = list(a=0.1, b=1)))
summary(fit3 <- lm(HTTCR86 ~ bs(DBH86, knots = c(70), degree = 2), data = p4))
## with(pp, hts <- c(DBH86, DBH87, )
summary(fit4 <- lm(HTTCR86 ~ bs(DBH86) + ELEVCL + ASPCL, data = pp))
## Below/above canopy


## Predict
dbhs <- seq(5, 45, length=100)
pred1 <- predict(fit, newdata = data.frame(DBH86=dbhs))
lines(dbhs, pred1, col = "blue", lwd =3)

dbhs <- seq(5, 45, length=100)
pred2 <- predict(fit3, newdata = data.frame(DBH86=dbhs))
lines(dbhs, pred2, col = "green", lwd =3)

abline(fit1, col = "green")

pred2 <- predict(fit2, newdata = data.frame(DBH86 = dbhs))
points(dbhs, pred2, col = "purple", type="l")

