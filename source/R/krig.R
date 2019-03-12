krig <- function(values, coords, grid, gv, distFUN=geo.dist, ...,
                     m=NA, cv=FALSE, neg.weights=TRUE, clamp=FALSE,
                     verbose=TRUE) {

    if (!is.vector(values)) stop("Values must be a vector.")

    if (!class(grid) == "data.frame") {
        ## Attempt coercion to data.frame
        grid <- as.data.frame(grid)
    }

    # more error checking
    if (length(values) != nrow(coords)) {
        stop('Number of values different from number of locations!')
    }

    mtest <- mtest.gv(gv)
    if (!mtest) stop("Object has no model built!")

    ## Check the arguments of the distFUN
    if (!all(c("from", "to") %in% names(formals(distFUN))))
        stop("The arguments of the distance function must be ",
             "\"distFUN(from, to, ...)\". See the examples for ",
             "details.")

    if (verbose) cat("Calculating distances...\n")
    ## Calculates distances betwen samples
    C <- as.matrix(distFUN(from=coords, to=coords, ...))

    ## Calculates distances between each pixel and sampled points
    ## NOTE: (from) pixels on rows, (to) samples on columns
    grdDist <- as.matrix(distFUN(from=grid, to=coords, ...))

    intpl <- .krig(values, C, grdDist, gv, m, clamp, neg.weights, verbose)

    if (cv) {
        cv <- .kCrossValid(values, C, gv, m, clamp)
        return(list(intpl=intpl, cross.valid=cv$cv, MSE=cv$MSE))
    } else {
    ## Return only interpolation
    return(intpl)
    }
}

.krig <- function(values, C, grdDist, gv, m=NA, clamp=FALSE,
                  neg.weights=TRUE, verbose=FALSE) {

    n <- length(values)   # number of samples
    ni <- nrow(grdDist) # number of locations to interpolate

    sill <- gv$model$sill

    Cc <- sill - predict(gv, C)

    if (is.na(m)) {
        if (verbose) cat("Ordinary Kriging\n")
        Cc <- rbind(cbind(Cc, rep(1, n)), c(rep(1, n), 0))
    } else {
        if (verbose) cat("Simple Kriging\n")
    }

    iC <- mpinv(Cc)

    Z <- sd <- rep(NA, ni)
    for (i in 1:ni) {

        if (verbose) cat(round(i/ni*100, 0), "%\r", sep="")

        D <- sill - predict(gv, grdDist[i,])

        if (is.na(m))
            D <- c(D, 1)

        w <- iC %*% D

        if (!neg.weights) {
            ## Negative weigths correction
            avg.w <- mean(abs(w)[w<0])
            avg.C <- mean(D[w<0])
            w[w<0] <- 0
            w[w < avg.w & D < avg.C] <- 0
            w <- w / sum(w)
        }

        if (is.na(m))
            Z[i] <- sum(values*w[1:n, 1]) else
            Z[i] <- m + sum((values-m)*w[,1])

        sd[i] <- (sill - sum(w[,1]*D))**0.5
    }

    if (verbose) cat("\n")

    if (clamp) {
        Z[Z > 1] = 1
        Z[Z < 0] = 0
    }

    return(data.frame(Z, sd))
}

.kCrossValid <- function(values, C, gv, m, clamp) {


    ## Perform cross validation and mean squared error
    cv <- matrix(NA, length(values), 2)
    colnames(cv) <- c('value', 'predicted')
    for (i in 1:length(values)) {
        cv.k <- .krig(values[-i], C[-i,-i], t(as.matrix(C[i,-i])),
                     gv=gv, m=m, clamp=clamp, verbose=FALSE)
        cv[i,] <- c(values[i], cv.k[1,1])
    }
    MSE <- sum(apply(cv, 1, diff)**2)/length(values)

    return(list(cv=cv, MSE=MSE))
}
