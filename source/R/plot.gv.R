plot.gv <-
function(x, line.res=100, pch=1,
         legend=TRUE, leg.x=NA, leg.y=NA, leg.cex=1,
         bar.length=0.1, bar.col="gray", bar.lty=par("lty"),
         xlab='Distance', ylab='Semivariance',
         x.line=3, y.line=3, ncol=1, main=NULL,
         leg.label = expression(italic('n')*' size'), ...) {
### TODO: check if it is a multi should be via a class attribut
### or in the object parameters slot.
### plot.gv can hadle multi and single gv objects

    ## check if gv is multi
    multi <- FALSE
    if ("gamma.mat" %in% names(x)) multi <- TRUE

    ## Get all distance classes to X-axis
    X <- x$lag
    Y <- x$gamma

    # Check if a model is present
    mtest <- mtest.gv(x)
    if (mtest) {
        xx <- seq(0, ceiling(max(X, na.rm=TRUE)),
                  length.out = line.res)
        yy <- predict(x, xx)
    }

    # Prepare point size relative to n
    lab.n <- range(x$n)
    cex <- 2 * x$n / lab.n[2]

    ## ### Begin plotting ### ##
    plot.new()

    ## Get X- and Y- axes ranges
    x.range <- c(0, max(X, na.rm=TRUE))
    mY <- max(Y, na.rm=TRUE)
    if (multi) mY <- max(x$gamma.ci[2,], na.rm=TRUE)
    if (mtest) {
        y.range <- c(0, max(c(mY,yy), na.rm=TRUE))
    } else {
        y.range <- c(0, mY)
    }

    plot.window(x.range, y.range)
    if (multi) {
        ##lines(X, x$gamma.ci[1,], lty=2)
        ##lines(X, x$gamma.ci[2,], lty=2)
        ##lines(X, Y, lty=1)
        ## Using bars instead of lines
        arrows(X, x$gamma.ci[1,], X, x$gamma.ci[2,], angle = 90,
               length = bar.length, code = 3, col = bar.col,
               lty = bar.lty)

    }
    points(X, Y, pch=pch, cex=cex*leg.cex, ...)

    if (mtest) lines(xx, yy, col='red', ...)

    axis(1)
    mtext(xlab, side=1, line=x.line)
    axis(2)
    mtext(ylab, side=2, line=y.line)
    box()

    # Plot Legend
    if (legend) {
        leg <- pretty(x$n)
        leg[1] <- 1
        if (is.na(leg.x) | is.na(leg.y))
            leg0 <- legend(0, 0, legend=leg, pch=pch, ncol=ncol,
                           pt.cex = leg.cex * 2 * leg / lab.n[2],
                           title = leg.label, plot=FALSE)
        if (is.na(leg.x)) leg.x = x.range[2] - leg0$rect$w - diff(x.range)*0.01
        if (is.na(leg.y)) leg.y = y.range[1] + leg0$rect$h + diff(y.range)*0.01
        legend(leg.x, leg.y, legend=leg, pch=pch, ncol=ncol,
               pt.cex = leg.cex * 2 * leg / lab.n[2],
               title = leg.label)
    }

    # Plot titles
    if(is.null(main)) {
        main <- "Semi-Variogram"
        if (mtest)
            main <- paste(main, "\n",
                        "Model:", x$model$type,
                        "Sill:", round(x$model$sill, 3),
                        "Range:", round(x$model$range, 3),
                        "Nugget:", round(x$model$nugget, 3))
    }

    title(main=main)
}
