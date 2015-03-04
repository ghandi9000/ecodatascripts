### predict_heights.R --- 
## Filename: predict_heights.R
## Description: Predict missing heights for trees from basal areas for Moosilauke
## Author: Noah Peart
## Created: Mon Mar  2 13:37:06 2015 (-0500)
## Last-Updated: Tue Mar  3 21:20:42 2015 (-0500)
##           By: Noah Peart
######################################################################
source("~/work/ecodatascripts/vars/heights/canopy.R")
## dat <- read.csv("~/work/data/moose/moose-wide.csv")

library(splines)
library(dplyr)

## Visualize
## with(can_dims, stripchart(time~ht, col = PPLOT, jitter=1))
plot <- c(13:15, 25:27)
p4 <- pp %>% filter(PPLOT %in% plot, STAT10 == "ALIVE") %>% select(PPLOT, SPLOT, ASPCL, ends_with("10"))
p4can <- can_dims %>% filter(PPLOT %in% plot, time == 10)

p4$ind <- ifelse(p4$ASPCL == "E", 1, 2)

with(p4, plot(DBH10, HTTCR10, pch = 15, col = ind))
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

