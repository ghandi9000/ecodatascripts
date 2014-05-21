##########################################################################
##
## Functions to convert x,y coordinates
##
##########################################################################

## Moosilauke x,y -> centered at origin [-4.5,4.5]
## Convert quadrat labels to be centered at origin of plot
## New locations range from -4.5 to 4.5 in x and y directions
## Note: returns NA for all quadrat numbers outside range [1,10]
center_quadrats <- function(x) {
    newx = seq(-4.5, 4.5, 1)
    xx = x
    for (i in 1:length(x))
        ifelse(!is.na(x[i]) && x[i] > 0 && x[i] <= 10,
               { xx[i] = newx[x[i]] }, { xx[i] = NA })
    return ( xx )
}
