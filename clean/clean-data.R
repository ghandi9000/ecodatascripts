### clean data applicable to allometry project
### Moosilauke Data
# remake the ba and growth columns so they are annualized and named
# with yr suffixes for long transformation
pp$BA86 <- 0.00007854*pp$DBH86*pp$DBH86 ### Basal area column, m^2
pp$BA87 <- 0.00007854*pp$DBH87*pp$DBH87
pp$BA98 <- 0.00007854*pp$DBH98*pp$DBH98
pp$BA10 <- 0.00007854*pp$DBH10*pp$DBH10

pp$BAGROWTH98 <- rep(NA, nrow(pp))
pp[!is.na(pp$BA86),]$BAGROWTH98 <- (pp[!is.na(pp$BA86),]$BA98-
                                    pp[!is.na(pp$BA86),]$BA86)/12
pp[!is.na(pp$BA87),]$BAGROWTH98 <- (pp[!is.na(pp$BA87),]$BA98-
                                    pp[!is.na(pp$BA87),]$BA87)/11
pp$BAGROWTH10 <- (pp$BA10-pp$BA98)/12

pp$DBHGROWTH98 <- rep(NA, nrow(pp))
pp[!is.na(pp$DBH86),]$DBHGROWTH98 <- (pp[!is.na(pp$DBH86),]$DBH98-
                                    pp[!is.na(pp$DBH86),]$DBH86)/12
pp[!is.na(pp$DBH87),]$DBHGROWTH98 <- (pp[!is.na(pp$DBH87),]$DBH98-
                                    pp[!is.na(pp$DBH87),]$DBH87)/11
pp$DBHGROWTH10 <-(pp$DBH10-pp$DBH98)/12

pp$HTGROWTH98 <- rep(NA, nrow(pp))
pp[!is.na(pp$HT86),]$HTGROWTH98 <- (pp[!is.na(pp$HT86),]$HT98-
                                    pp[!is.na(pp$HT86),]$HT86)/12
pp[!is.na(pp$HT87),]$HTGROWTH98 <- (pp[!is.na(pp$HT87),]$HT98-
                                    pp[!is.na(pp$HT87),]$HT87)/11
pp$HTGROWTH10 <-(pp$HT10-pp$HT98)/12

# remove old growth columns
todrop <- c("BAGROWTH1","BAGROWTH2","BAGROWTH3","DBHGROWTH1","DBHGROWTH2",
            "DBHGROWTH3","HTGROWTH1","HTGROWTH2","HTGROWTH3")
pp <- subset(pp, select = -c(which(names(pp) %in% todrop)))

# Make prior columns for ht, dbh, ba
pp$PRIORDBH86 <- rep(NA, nrow(pp))
pp$PRIORDBH87 <- rep(NA, nrow(pp))
pp$PRIORDBH98 <- rep(NA, nrow(pp))
pp$PRIORDBH10 <- rep(NA, nrow(pp))
pp[!is.na(pp$DBH86),]$PRIORDBH98 <- pp[!is.na(pp$DBH86),]$DBH86
pp[!is.na(pp$DBH87),]$PRIORDBH98 <- pp[!is.na(pp$DBH87),]$DBH87
pp[!is.na(pp$DBH98),]$PRIORDBH10 <- pp[!is.na(pp$DBH98),]$DBH98

pp$PRIORBA86 <- rep(NA, nrow(pp))
pp$PRIORBA87 <- rep(NA, nrow(pp))
pp$PRIORBA98 <- rep(NA, nrow(pp))
pp$PRIORBA10 <- rep(NA, nrow(pp))
pp[!is.na(pp$BA86),]$PRIORBA98 <- pp[!is.na(pp$BA86),]$BA86
pp[!is.na(pp$BA87),]$PRIORBA98 <- pp[!is.na(pp$BA87),]$BA87
pp[!is.na(pp$BA98),]$PRIORBA10 <- pp[!is.na(pp$BA98),]$BA98

pp$PRIORHT86 <- rep(NA, nrow(pp))
pp$PRIORHT87 <- rep(NA, nrow(pp))
pp$PRIORHT98 <- rep(NA, nrow(pp))
pp$PRIORHT10 <- rep(NA, nrow(pp))
pp[!is.na(pp$HT86),]$PRIORHT98 <- pp[!is.na(pp$HT86),]$HT86
pp[!is.na(pp$HT87),]$PRIORHT98 <- pp[!is.na(pp$HT87),]$HT87
pp[!is.na(pp$HT98),]$PRIORHT10 <- pp[!is.na(pp$HT98),]$HT98

# make columns that identify direction for dbh and ht from last sampling period
# period 1
pp$p98dbh <- rep(NA, nrow(pp))
pp[pp$PPLOT<16,]$p98dbh <- pp[pp$PPLOT<16,]$DBH98 > pp[pp$PPLOT<16,]$DBH86
pp[pp$PPLOT>=16,]$p98dbh <- pp[pp$PPLOT>=16,]$DBH98 > pp[pp$PPLOT>=16,]$DBH87
pp$p98ht <- rep(NA, nrow(pp))
pp[pp$PPLOT<16,]$p98ht <- pp[pp$PPLOT<16,]$HT98 > pp[pp$PPLOT<16,]$HT86
pp[pp$PPLOT>=16,]$p98ht <- pp[pp$PPLOT>=16,]$HT98 > pp[pp$PPLOT>=16,]$HT87
# period 2
pp$p10dbh <- pp$DBH10 > pp$DBH98
pp$p10ht <- pp$HT10 > pp$HT98

### Doug Fir Data
# make unique plots: combine INSTALL with PLOT
df$PPLOT <- paste(df$INSTALL,df$PLOT,sep = ".")

## remove columns ending in 0
zcols <- grep("0$", names(df))
df <- df[,-zcols]

## make names same as moose data
change <- c("TREE","SPP")
names(df)[names(df) %in% change] <- c("TAG","SPEC")

## numbers to yrs
yrs <- c(73, 76, 79, 82, 85, 91, 97)
yrs <- sapply(yrs, as.character)
for(i in 1:length(yrs)) {
    x <- grep(paste0("[[:alpha:]]",i,"$"),names(df))
    names(df)[x] <- gsub(paste0(i,"$"), yrs[i], names(df)[x])
}

## want BA, annualized growth, and prior columns
yrs <- c(73, 76, 79, 82, 85, 91, 97)
for(i in yrs) {
    df[,paste0("BA",i)] <- 0.00007854*df[,paste0("DBH",i)]^2
}

df$DBHGROWTH73 <- rep(NA,nrow(df))
df$BVGROWTH73 <- rep(NA,nrow(df))
df$BAGROWTH73 <- rep(NA,nrow(df))
df$HTGROWTH73 <- rep(NA,nrow(df))
df$PRIORDBH73 <- rep(NA,nrow(df))
df$PRIORBV73 <- rep(NA,nrow(df))
df$PRIORBA73 <- rep(NA,nrow(df))
df$PRIORHT73 <- rep(NA,nrow(df))

for(i in 2:length(yrs)) {
    print(i)
    df[,paste0("DBHGROWTH",yrs[i])] <- (df[,paste0("DBH",yrs[i])] -
                                        df[,paste0("DBH",yrs[i-1])])/(yrs[i]-yrs[i-1])
    df[,paste0("PRIORDBH",yrs[i])] <- df[,paste0("DBH",yrs[i-1])]
    df[,paste0("BAGROWTH",yrs[i])] <- (df[,paste0("BA",yrs[i])] -
                                       df[,paste0("BA",yrs[i-1])])/(yrs[i]-yrs[i-1])
    df[,paste0("PRIORBA",yrs[i])] <- df[,paste0("BA",yrs[i-1])]
    df[,paste0("HTGROWTH",yrs[i])] <- (df[,paste0("HT",yrs[i])] -
                                       df[,paste0("HT",yrs[i-1])])/(yrs[i]-yrs[i-1])
    df[,paste0("PRIORHT",yrs[i])] <- df[,paste0("HT",yrs[i-1])]
    df[,paste0("BVGROWTH",yrs[i])] <- (df[,paste0("BV",yrs[i])] -
                                        df[,paste0("BV",yrs[i-1])])/(yrs[i]-yrs[i-1])
    df[,paste0("PRIORBV",yrs[i])] <- df[,paste0("BV",yrs[i-1])]
}

