plot.gv <-
function(x, line.res=100, pch=1,
         legend=TRUE, leg.x=NA, leg.y=NA, leg.cex=1, ...) {
### TODO: check if it is a multi should be via a class attribut
### or in the object parameters slot.
### plot.gv activates either the single plotting or the multi ploting
### although I still don't know if it should have different functions


    ## check if is multi gv
    multi <- FALSE
    if (is.matrix(x$gamma)) multi <- TRUE
    
    X <- x$lag[!is.na(x$lag)]
    if (multi) {
        Y <- apply(x$gamma, 1, median)
        mask <- !is.na(Y)
        Y <- Y[mask]
        
        ## 95% confidence interval for the median with
        ## approximation to normal distribution
        m <- ncol(x$gamma)*0.5
        sd <- sqrt(m*0.5)
        Z <- 1.959964 # qnorm(1-alpha/2) for alpha = 0.05
        ci <- c(round(m-Z*sd, 0), round(m+Z*sd, 0)+1)
        cint <- apply(x$gamma[mask,], 1,
                      function(x, ci) sort(x)[ci], ci=ci)
    } else {
        Y <- x$gamma[!is.na(x$gamma)]
    }
    
    mtest <- mtest.gv(x)
    if (mtest) {
        xx <- seq(0, ceiling(max(X)), length.out = line.res)
        yy <- predict(x, xx)
    }

    #Prepare point size relative to n
    lab.n <- range(x$n)
    cex <- 2 * x$n / lab.n[2]
    
    # Begin plot
    plot.new()
    x.range <- c(0, max(X))
    mY <- max(Y)
    if (multi) mY <- max(cint[2,])
    if (mtest) {
        y.range <- c(0, max(c(mY,yy)))
    } else {
        y.range <- c(0, mY)
    }
    print(mY)
    print(x.range)
    print(y.range)
    
    plot.window(x.range, y.range)
    if (multi) {
        YY <- x$gamma[,1]
        points(X, YY[!is.na(YY)], cex = leg.cex,
               col = rgb(1,0,0,0.1), pch=16)      
        for (i in 2:ncol(x$gamma)) {
            YY <- apply(x$gamma[,1:i], 1, median)
            points(X, YY[!is.na(YY)], cex = leg.cex,
                   col = rgb(0,0.5,0.7,0.05), pch=16)
        }
        lines(X, cint[1,], lty=2)
        lines(X, cint[2,], lty=2)
        lines(X, Y, lty=1)
    }
    
    points(X,Y, pch=pch, cex=cex*leg.cex, ...)

    if (mtest) lines(xx, yy, col='red', ...)

    axis(1)
    axis(2)
    box()

    # Plot Legend
    if (legend) {
        leg <- pretty(x$n)
        leg[1] <- 1
        if (is.na(leg.x)) leg.x = 0.9 * diff(x.range)
        if (is.na(leg.y)) leg.y = 0.06 * length(leg) * diff(y.range)
        legend(leg.x, leg.y, legend=leg, pch=pch, 
               pt.cex = leg.cex * 2 * leg / lab.n[2],
               title = expression(italic('n')*' size'))
    }

    # Plot titles
    main <- "Semi-Variogram"
    if (mtest) 
        main <- paste(main, "\n",
                      "Model:", x$model$type,
                      "Sill:", round(x$model$sill, 3),
                      "Range:", round(x$model$range, 3),
                      "Nugget:", round(x$model$nugget, 3))
    title(main=main, xlab='Distance', ylab='Semivariance')
}


