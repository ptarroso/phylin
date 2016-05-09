predict.gv <-
function(object, newdata, ...) {

    if (missing(newdata) || is.null(newdata)) {
        newdata <- object$lag[!is.na(object$lag)]
    }

    mtest <- mtest.gv(object)
    if (!mtest) stop("Object has no model built!")

    model  <- object$model$type
    S <- object$model$sill
    N <- object$model$nugget
    R <- object$model$range    

    if (model == "gaussian") {
        vFUN <- function(x, S, N, R) {
            N + (S-N) * (1-exp(-3*(x**2)/(R**2)))
        }

    } else if (model == "exponential") {
        vFUN <- function(x, S, N, R) {
            N + (S-N) * (1-exp(-3*x/R))
        }
        
    } else if (model == "spherical") {
        vFUN <- function(x, S, N, R) {
            ifelse(x<=R,
                   N+(S-N)*(((3*(x))/(2*R))-((x)**3/(2*R**3))),
                   S)
        }
        
    } else if (model == "pentaspherical") {
        vFUN <- function(x, S, N, R) {
            ifelse(x<=R, 
                   N+(S-N)*(((15/8)*(x/R)) - ((5/4)*(x/R)**3) + ((3/8)*(x/R)**5)),
                   S)
        }
        
    } else if (model == "cubic") {
        vFUN <- function(x, S, N, R) {
            ifelse(x<=R,
                   N+(S-N)*(7*(x/R)**2 - (35/4)*(x/R)**3 + (7/2)*(x/R)**5 - (3/4)*(x/R)**7),
                   S)
        }
        
    } else if (model == "linear") {
        vFUN <- function(x, S, N, R) {
            ifelse(x<=R, ((S-N)/R)*x+N, S)
        }
    }

    y <- vFUN(newdata, S, N, R)
    y
}
