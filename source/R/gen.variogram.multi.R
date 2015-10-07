gen.variogram.multi <-
function(x, multiY, lag = mean(x)/sqrt(nrow(x)), tol=lag/2,
         lmax = NA, verbose=TRUE) {
### This is an experimental version of variogram that takes into
### account the uncertainty from the all the posterior trees generated
### It is made not do directly depend on 'ape' package, although it
### will be needed to prepare the distance matrices before.
### Note than the tree tip labels must be identical to the real
### distance column and row labels.

### TODO: This should be a hidden function to gen.variogram, if a
### list (of distance matrices) is given instead of distance matrix.
### Maybe also simplify the gen.variogram just to parse arguments
### and add an aditional function that will do a variogram
### Must add information to the class, to descriminate if it is
### multi or single!
    
    ## some variable checking
    if (!(class(x) == "matrix"))
        x <- as.matrix(x)
    
    if (!class(multiY) == "list")
        stop(paste("multiY must be a list with multiple genetic",
                   " distance matrices."))

    ## Variable to hold the distribution of values
    distrib <- NULL
    n <- length(multiY)

    for (i in 1:n) {
        y <- multiY[[i]]
        
        ## attempt to order the genetic distance with real dist mat
        y <- y[rownames(x), colnames(x)]

        ## build the variogram
        gv <- gen.variogram(x, y, lag=lag, tol=tol, lmax=lmax)

        ## Initialize the distrib
        if (is.null(distrib))
            distrib <- matrix(NA, length(gv$gamma), n)

        ## Populate distribution
        distrib[,i] <- gv$gamma
        
        if (verbose) {
            ## Displays the error evolution
            ## error is the sum of squared difference of the median
            ii <- formatC(i, width = nchar(n), format = "d",
                          flag = "0")
            if (i > 1) {
                newMedian <- apply(distrib[,1:i], 1, median)
                err <- sum((newMedian - oldMedian)**2)
                cat(ii, ' - ', err, "\n")
                oldMedian <- newMedian
            } else {
                cat(ii, ' - ------------\n')
                oldMedian <- distrib[,i]
            }
        }
    }
    gv$gamma <- distrib
    return(gv)        
}
