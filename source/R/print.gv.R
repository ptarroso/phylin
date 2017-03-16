print.gv <-
function(x, ...) {
    ## check if gv is multi
    multi <- FALSE
    if ("gamma.mat" %in% names(x)) multi <- TRUE

    line1 <- "Variogram"
    if (multi) {
        line1 <- paste(line1, "with multiple genetic distance matrices.")
    } else {
        line1 <- paste(line1, "with a single genetic distance matrix.")
    }

    n.obs <- nrow(x$x)
    n.dstcl <- length(x$lag)
    line2 <- paste(n.obs, "observations and", n.dstcl,
                   "distance classes (from", round(min(x$x), 2), "to",
                   round(max(x$x), 2), ")")
    line3 <- paste(round(x$param$lag, 2), "lag size with",
                   round(x$param$tol, 2), "tolerance.")

    # Check if a model is present
    mtest <- mtest.gv(x)
    if (mtest) {
        line4 <- paste("A", x$model$type, "model is fitted with", x$model$sill,
                       "sill,", x$model$range, "range and", x$model$nugget,
                       "nugget.")
    } else {
        line4 <- "No model fitted"
    }


    cat(line1, "\n", line2, "\n", line3, "\n", line4, "\n")

}
