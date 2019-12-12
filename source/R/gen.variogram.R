gen.variogram <-
function(x, y, lag = quantile(as.matrix(x), 0.05), tol=lag/2, lmax = NA,
         bootstraps = 999, verbose = FALSE) {

    ## Check x class status and attempt coercion to dist
    if (!("dist" %in% class(x)))
            x <- as.dist(x)

    ## Check if there are multiple genetic distances and
    ## attempt coercion to dist
    multi <- FALSE
    if ("list" %in% class(y)) {
        multi <- TRUE
        for (i in 1:length(y)) {
            if (!("dist" %in% class(y[[i]])))
                y[[i]] <- as.dist(y[[i]])

            if (!all(dim(y[[i]]) == dim(x)))
                stop("Dimensions of x and y do not match.")
       }
    } else {
        if (!("dist" %in% class(y)))
            y <- as.dist(y)

        if (!all(dim(y) == dim(x)))
            stop("Dimensions of x and y do not match.")
    }

    if (multi) {
        gv <- .gen.variogram.multi(x, y, lag = lag, tol = tol,
                                  lmax = lmax,
                                  bootstraps = bootstraps,
                                  verbose = verbose)
    } else {
        gv <- .gen.variogram.single(x, y, lag = lag, tol = tol,
                                   lmax = lmax)
    }

    ## Throw a warning if lag used generates NAs
    if (sum(is.na(gv$gamma)) > 0) {
        warning("The variogram contains NAs.",
                "Consider adjusting the lag.")
    }

    gv
}
