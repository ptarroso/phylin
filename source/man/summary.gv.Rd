\name{summary.gv}
\alias{summary.gv}
\title{
    Summary for 'gv' object
}
\description{
    Displays general information about the 'gv' object.
}
\usage{
\method{summary}{gv}(object, ...)
}
\arguments{
  \item{object}{
    'gv' object as given by 'gen.variogram' or 'gv.model'.
}
  \item{\dots}{
    Further plotting arguments to be passed.
}
}
\value{
    Print summary table.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\seealso{
    \code{\link{gen.variogram}}
    \code{\link{gv.model}}
}
\examples{
data(vipers)
data(d.gen)

# create a distance matrix between samples
r.dist <- dist(vipers[,1:2])

# fit a variogram with defaults (shperical model) and estimation of range
gv <- gen.variogram(r.dist, d.gen)

#plot semi-variogram
summary(gv)

# plot semi-variogram with model
gv <- gv.model(gv)
summary(gv)
}
\keyword{ variogram }
\keyword{ kriging }
