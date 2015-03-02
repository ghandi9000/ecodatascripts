## Preparatory moosilauke data work
## NOTE: using estimated heights and bole volumes from boles project
## "~/work/boles/"
## eht = height indicator column, 0 means not predicted
source("~/work/ecodatascripts/read/read-moose.R") ## calls data 'pp'
source("~/work/functions/functions-datatrans.R")

## Check to see that we have the necessary columns
cols <- c("DBH","HT","EHT","BV")
yrs <- c(86, 87, 98, 10)
checkCols(pp, cols, yrs) ## returns columns that arent present

## rename columns that have been changed
changed <- c("HTTCR", "ebv")
replacements <- c("HT", "BV")
for (i in 1:length(changed))
    names(pp) <- gsub(changed[i], replacements[i], names(pp))
checkCols(pp, cols, yrs) ## recheck cols

## remake the ba and growth columns so they are annualized and named
## with yr suffixes for long transformation
pp$BA86 <- 0.00007854*pp$DBH86*pp$DBH86 ### Basal area column, m^2
pp$BA87 <- 0.00007854*pp$DBH87*pp$DBH87
pp$BA98 <- 0.00007854*pp$DBH98*pp$DBH98
pp$BA10 <- 0.00007854*pp$DBH10*pp$DBH10

## Initialize derived columns:
## BAGROWTH, DBHGROWTH, HTGROWTH, PRIORBA, PRIORDBH, PRIORHT
pp[, c(paste0("BAGROWTH", yrs), paste0("DBHGROWTH", yrs), paste0("HTGROWTH", yrs))] <- NA
pp[, c(paste0("PRIORDBH", yrs), paste0("PRIORBA", yrs), paste0("PRIORHT", yrs))] <- NA

## Basal/DBH area growth indices
grew86_98 <- which(!is.na(pp$BA86) & !is.na(pp$BA98))
ht86_98 <- which(!is.na(pp$HT86) & !is.na(pp$HT98))
grew87_98 <- which(!is.na(pp$BA87) & !is.na(pp$BA98))
ht87_98 <- which(!is.na(pp$HT87) & !is.na(pp$HT98))
grew98_10 <- which(!is.na(pp$BA98) & !is.na(pp$BA10))
ht98_10 <- which(!is.na(pp$HT98) & !is.na(pp$HT10))

## First growth period (86-98, 87-98)
pp[grew86_98, ]$BAGROWTH98 <- (pp[grew86_98, ]$BA98 - pp[grew86_98, ]$BA86)/12
pp[grew87_98, ]$BAGROWTH98 <- (pp[grew87_98, ]$BA98 - pp[grew87_98, ]$BA87)/11
pp[grew86_98, ]$DBHGROWTH98 <- (pp[grew86_98, ]$DBH98 - pp[grew86_98, ]$DBH86)/12
pp[grew87_98, ]$DBHGROWTH98 <- (pp[grew87_98, ]$DBH98 - pp[grew87_98, ]$DBH87)/11
pp[ht86_98, ]$HTGROWTH98 <- (pp[ht86_98, ]$HT98 - pp[ht86_98, ]$HT86)/12
pp[ht87_98, ]$HTGROWTH98 <- (pp[ht87_98, ]$HT98 - pp[ht87_98, ]$HT87)/11

## Second growth period (98-10)
pp[grew98_10, ]$BAGROWTH10 <- (pp[grew98_10, ]$BA10 - pp[grew98_10, ]$BA98)/12
pp[grew98_10, ]$DBHGROWTH10 <- (pp[grew98_10, ]$DBH10 - pp[grew98_10, ]$BA98)/12
pp[grew98_10, ]$HTGROWTH10 <- (pp[grew98_10, ]$HT10 - pp[grew98_10, ]$HT10)/12

## Make prior columns
pp[!is.na(pp$DBH86),]$PRIORDBH98 <- pp[!is.na(pp$DBH86),]$DBH86
pp[!is.na(pp$DBH87),]$PRIORDBH98 <- pp[!is.na(pp$DBH87),]$DBH87
pp[!is.na(pp$DBH98),]$PRIORDBH10 <- pp[!is.na(pp$DBH98),]$DBH98

pp[!is.na(pp$BA86),]$PRIORBA98 <- pp[!is.na(pp$BA86),]$BA86
pp[!is.na(pp$BA87),]$PRIORBA98 <- pp[!is.na(pp$BA87),]$BA87
pp[!is.na(pp$BA98),]$PRIORBA10 <- pp[!is.na(pp$BA98),]$BA98

pp[!is.na(pp$HT86),]$PRIORHT98 <- pp[!is.na(pp$HT86),]$HT86
pp[!is.na(pp$HT87),]$PRIORHT98 <- pp[!is.na(pp$HT87),]$HT87
pp[!is.na(pp$HT98),]$PRIORHT10 <- pp[!is.na(pp$HT98),]$HT98

## colnames to lower case and drop unwanted columns
names(pp) <-tolower(names(pp))

## separate cht8687 into cht86 and cht87:
## plots 1-12 have cht86 and 13-24 have cht87
pp$cht86 <- ifelse(pp$pplot < 16, pp$cht8687, NA)
pp$cht87 <- ifelse(pp$pplot > 15, pp$cht8687, NA)

## Columns to keep
toKeep <- c("pplot","splot","tag","spec","yrmort","elev","elevcl","asp","aspcl",
            "bqudx","bqudy","soilcl","slopcl","slope8687", paste0("cht",yrs),
            paste0("stat",yrs), paste0("dbh",yrs), paste0("ba",yrs),
            paste0("ht",yrs), paste0("eht",yrs), paste0("decm",yrs), paste0("cpos",yrs),
            paste0("dbhgrowth",yrs), paste0("htgrowth",yrs), paste0("priordbh",yrs),
            paste0("priorba",yrs), paste0("bagrowth",yrs), paste0("priorht",yrs),
            paste0("clong",yrs), paste0("cperp",yrs), paste0("crht",yrs))

pp <- pp[, names(pp) %in% toKeep]

## Remove rows where aspcl/elevcl == "" and drop blank levels
pp <- droplevels(pp[-which(pp$elevcl == "" | pp$aspcl == ""), ])

## Convert all tags related to unidentified species to ""
pp[pp$spec == "UNID", "spec"] <- ""
pp <- droplevels(pp)

## Write data
write.csv(pp, "~/work/data/moose/moose-wide.csv", row.names = FALSE)

## cleanup
rm(list=ls())
