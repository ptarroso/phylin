mtest.gv <-
function(gv) {
    # test if 
    if (class(gv) != 'gv') stop("Object must be of class 'gv'")

    if (length(gv$model) == 1) {
        if (is.na(gv$model)) {
            mtest <- FALSE
        } else {
            stop("Model in 'gv' has no parameters")
        }
    } else if (length(gv$model) == 4) {
        mtest <- TRUE
    } else {
        stop("Wrong number of model parameters. Three were expected.")
    }
    mtest
}
