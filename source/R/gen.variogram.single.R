.gen.variogram.single <-
function(x, y, lag, tol, lmax = NA) {

    if (is.na(lmax)) lmax = max(x, na.rm=TRUE)
    lagv <- seq(0, lmax, lag)
    gamma <- n <- rep(NA, length(lagv))

    for (i in 1:length(lagv) )
    {
        l <- lagv[i]

        #remove duplicates from distance matrix
        il <- which(x > l-tol & x <= l+tol)

        n[i] <- length(il)
        if (n[i] != 0) {
            gamma[i] <- sum(y[il]**2)/(n[i]*2)
            lagv[i] <- mean(x[il])
        } else {
            gamma[i] <- lagv[i] <- NA
        }
    }

    # create object gv
    gv <- list(model=NA, x=x, y=y, lag=lagv, gamma=gamma, n=n,
               param=list(lag=lag, tol=tol, lmax=lmax))
    class(gv) <- 'gv'

    gv
}
