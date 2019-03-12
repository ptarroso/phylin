\name{print.gv}
\alias{print.gv}
\title{
    Prints details of a 'gv' object
}
\description{
  The function is used when a 'gv' object is called.
}
\usage{
\method{print}{gv}(x, ...)
}
\arguments{
  \item{x}{
    'gv' object as given by 'gen.variogram'.
}
\item{\dots}{
  Further plotting arguments to be passed.
}
}
\details{
    This prints the details of a 'gv' object including number of observations
    and other variogram creation parameters used. It will also display model
    details if a model was fitted to the empirical variogram.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\seealso{
    \code{\link{gen.variogram}}
}
\examples{
data(vipers)
data(d.gen)

# create a distance matrix between samples
r.dist <- dist(vipers[,1:2])

# fit a variogram with defaults (shperical model) and estimation of range
gv <- gen.variogram(r.dist, d.gen, 0.25)

# print variogram details
gv

# add a model to variogram
gv <- gv.model(gv)

# print variogram with model details
gv
}
\keyword{ variogram }
\keyword{ kriging }
