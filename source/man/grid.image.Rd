\name{grid.image}
\alias{grid.image}
\title{
    Simple plot of interpolated grid.
}
\description{
    Plots the interpolated grid.
}
\usage{
grid.image(intpl, grid, breaks=10, ic=1, colFUN=heat.colors, 
           main=colnames(intpl)[ic], xlab=colnames(grid)[1], 
           ylab=colnames(grid)[2], sclab=NA, ...)
}
\arguments{
  \item{intpl}{
    A matrix or vector with interpolation results.
}
  \item{grid}{
    A table containing longitude and latitude of interpolated locations.
}
  \item{breaks}{
    Number of breaks in the scale.
}
  \item{ic}{
    Column index or name from 'intpl' table to show. Defaults to the first 
    column. Can be used to plot standard deviation or any other column. This
    value is ignored of 'intpl' is a vector.
}
  \item{colFUN}{
    Function to process colors. Can be any of R base color functions (e.g.
    \code{\link{rainbow}}, \code{\link{terrain.colors}}, etc) or user defined
    function.
}
  \item{main}{
    Main title.
}
  \item{xlab}{
    X axis label. Defaults to name of the first 'grid' column.
}
  \item{ylab}{
    Y axis label. Defaults to name of the secont 'grid' column.
}
  \item{sclab}{
    Scale label to plot under the scale bar.
}
  \item{...}{
    Futher arguments to be passed to par. Most used is 'cex' to control the 
    font size.
}
}
\details{
    This function may be used to produce a simple plot of the interpolated
    grid. It has some customizable features and it plots a scale bar of the
    Z values shown.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\note{
    Does not work with multiple plots (e.g. with 'layout').
}
\seealso{
    \code{\link{image}}
    \code{\link{krig}}
    \code{\link{idw}}
}
\examples{
    data(vipers)
    data(d.gen)

    # create a grid of the sampled area for inteprolation
    grid <- expand.grid(x=seq(-9.5,3,0.25), y=seq(36, 43.75, 0.25))

    # create a distance matrix between samples
    r.dist <- dist(vipers[,1:2])

    # fit a model with defaults (shperical model) and estimation of range
    gv <- gen.variogram(r.dist, d.gen, 0.25)
    gv <- gv.model(gv)

    # interpolation of the distances to first sample with ordinary kriging
    int.krig <- krig(d.gen[,1], vipers[,1:2], grid, gv)

    #plot the interpolation results
    grid.image(int.krig, grid, main='Krigging Interpolation', 
               xlab='Longitude',ylab = 'Latitude', 
               sclab=paste('Genetic distance to sample', 
               colnames(d.gen)[1]))

    # User can add extra elements to the main plot.
    points(vipers[,1:2], cex=d.gen[,1]*15+0.2) 

}
\keyword{ interpolation }
\keyword{ kriging }
\keyword{ idw }
\keyword{ image }
