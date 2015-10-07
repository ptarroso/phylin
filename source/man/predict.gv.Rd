\name{predict.gv}
\alias{predict.gv}
\title{
    Predict method for 'gen.variogram' object with model.
}
\description{
    Predicts values based on a fitted gen.variogram model.
}
\usage{
\method{predict}{gv}(object, newdata, ...)
}
\arguments{
  \item{object}{
    'gv' fitted model (see 'gen.variogram').
}
  \item{newdata}{
    Real distances matrix to predict genetic distance by the fitted model.
}
  \item{\dots}{
    Further arguments to be passed.
}
}
\value{
   Returns the matrix of predicted genetic distances. 
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\seealso{
    \code{\link{gen.variogram}}
    \code{\link{plot.gv}}
    \code{\link{krig}}
}
\examples{
data(vipers)
data(d.gen)

# create a grid of the sampled area for interpolation
grid <- expand.grid(x=seq(-10,10,0.5), y=seq(30, 50, 0.5))

# create a distance matrix between samples
r.dist <- dist(vipers[,1:2])

# fit a variogram with defaults (spherical model) and estimation of range
gv <- gen.variogram(r.dist, d.gen)
gv <- gv.model(gv)

all.dist <- as.matrix(dist(grid))

result <- predict(gv, all.dist)

}
\keyword{ predict }
\keyword{ variogram }
\keyword{ kriging }
