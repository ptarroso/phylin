.gen.variogram.multi <-
function(x, multiY, lag, tol, lmax = NA, bootstraps = 999, verbose=TRUE) {

    ## Variable to hold the distribution of values
    distrib <- NULL
    n <- length(multiY)

    for (i in 1:n) {
        y <- multiY[[i]]

        ## build the variogram
        gv <- .gen.variogram.single(x, y, lag=lag, tol=tol, lmax=lmax)

        ## Initialize the distrib
        if (is.null(distrib))
            distrib <- matrix(NA, length(gv$gamma), n)

        ## Populate distribution
        distrib[,i] <- gv$gamma

        if (verbose) {
            ## Displays the error evolution
            ## error is the sum of squared differences of the median
            ii <- formatC(i, width = nchar(n), format = "d",
                          flag = "0")
            if (i > 1) {
                newMedian <- apply(distrib[,1:i], 1, median)
                err <- sum((newMedian - oldMedian)**2, na.rm = TRUE)
                cat(ii, ' - ', err, "\n")
                oldMedian <- newMedian
            } else {
                cat(ii, ' - ------------\n')
                oldMedian <- distrib[,i]
            }
        }
    }
    gv$gamma.mat <- distrib
    gv$gamma <- apply(distrib, 1, median)

    if (!is.null(bootstraps)) {
        if (!is.numeric(bootstraps))
            stop("Bootstraps must be numeric.")

        ## Do some bootstraps to calculate the 95% confidence
        ## interval for the median
        bootstrp <- matrix(NA, bootstraps+1, length(gv$gamma))
        bootstrp[bootstraps+1,] <- gv$gamma
        for (bt in 1:bootstraps) {
            sampleBT <- apply(distrib, 1, sample, replace=TRUE)
            bootstrp[bt,] <- apply(sampleBT, 2, median)
        }
        gv$gamma.ci <- apply(bootstrp, 2, quantile,
                             probs=c(0.05, 0.975), na.rm =TRUE)

    }

    return(gv)
}
