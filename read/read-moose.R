## Just tracks the where the master data for Moosilauke lives
if (Sys.info()['sysname'] == "Linux") {
    pp <- read.csv("~/Dropbox/Shared/Data/pptreemas10bv.csv")
} else
    pp <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10bv.csv")
