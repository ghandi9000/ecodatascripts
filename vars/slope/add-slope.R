## Add slope column to moose data
## - Just converts SLOPE8687 from slope percentage to degrees

## Converts slope percentage to slope in degrees
slpperc_todeg <- function(slpperc) {
    return ( atan( slpperc / 100) * (360/(2*pi)) )
}
