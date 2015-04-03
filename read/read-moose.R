## Just tracks the where the master data for Moosilauke lives
if (Sys.info()['sysname'] == "Linux") {
    pp <- read.csv("~/Dropbox/Shared/Data/pptreemas10.csv")
} else if ("noah" %in% list.files("C:\\Users")) {
    pp <- read.csv("C:/Users/noah/Dropbox/Shared/Data/pptreemas10.csv")
} else {
    cat("\n\n\t\t*** Couldn't find moosilauke data! ***
\n\t\tSet the location in 'ecodatascripts/read-moose.R'.\n\n")
    
    ##
    ##                          Set path to data here!
    ##
    
    ## Fill in path and uncomment the following line
    ## pp <- read.csv("path/to/data")

    stop("Not run.")  # remove this
}
