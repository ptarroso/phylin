gv.model <- function(gv, model='spherical', sill=NA, 
                     range=NA, nugget=0, ctrl=nls.control()) {

    dt <- data.frame(x=gv$lag[!is.na(gv$lag)], 
                     y=gv$gamma[!is.na(gv$gamma)])

    models <- c("gaussian", "exponential", "spherical",
                "pentaspherical", "cubic", "linear")
    model  <- match.arg(model, models)

    # add additional info to object gv
    if(!mtest.gv(gv)) gv$model <- list()
    gv$model$type <- model

    fit <- NULL

    ## Prepare start list for nls
    start <- list()
    if (is.na(sill)) 
        start[['S']] <- max(dt$y) else S <- sill
    if (is.na(range))
        start[['R']] <- min(dt$x[dt$y == max(dt$y)]) else R <- range
    if (is.na(nugget))
        start[['N']] <- 0 else N <- nugget
    
    if (length(start) > 0) {
        if (model == "gaussian") {
            ## Fit Gaussian model to empirical variogram
            fit <- nls(y ~ N + (S-N) * (1-exp(-3*(x**2)/(R**2))),
                       data=dt, control=ctrl, 
                       start=start)
            
        } else if (model == "exponential") {
            ## Fit exponential model (decay increasing form)
            fit <- nls(y ~ N + (S-N)*(1-exp(-3*x/R)),
                       data=dt, control=ctrl, 
                       start=start)
            
        } else if (model == "spherical") {
            ## Fit spherical to empirical variogram
            fit <- nls(y ~ ifelse(x<=R, N+(S-N)*(((3*(x))/(2*R))-((x)**3/(2*R**3))), S),
                       data=dt, control=ctrl, 
                       start=start)
            
        } else if (model == "pentaspherical") {
            ## Fit pentaspherical to empirical variogram
            fit <- nls(y ~ ifelse(x<=R, N+(S-N)*(((15/8)*(x/R)) - ((5/4)*(x/R)**3) + ((3/8)*(x/R)**5)), S),
                       data=dt, control=ctrl, 
                       start=start)
            
        } else if (model == "cubic") {
            ## Fit cubic model to empirical variogram
            fit <- nls(y ~ ifelse(x<=R, N+(S-N)*(7*(x/R)**2 - (35/4)*(x/R)**3 + (7/2)*(x/R)**5 - (3/4)*(x/R)**7), S),
                       data=dt, control=ctrl, 
                       start=start)
            
        } else if (model == "linear") {
            ## Fit linear model to empirical variogram
            fit <- nls(y ~ ifelse(x<=R, ((S-N)/R)*x+N, S),
                       data=dt, control=ctrl, 
                       start=start)
        }
    }

    cf <- coef(fit)
    if ('S' %in% names(cf)) 
        gv$model$sill <- cf[['S']] else gv$model$sill <- S
    if ('N' %in% names(cf)) 
        gv$model$nugget <- cf[['N']] else gv$model$nugget <- N
    if ('R' %in% names(cf)) 
        gv$model$range <- cf[['R']] else gv$model$range <- R
    
    class(gv) <- 'gv'

    # add residuals to gv
    pred <- predict(gv)
    residuals <- dt$y - pred
    gv$residuals <- residuals

    gv
}

