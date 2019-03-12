\name{plot.gv}
\alias{plot.gv}
\title{
    Plot a 'gv' object
}
\description{
  Plot the semi-variogram in a gv object. If a multiple genetic
  distances are found, it plots the median value and the 95\% confidence
  interval for the median.
}
\usage{
\method{plot}{gv}(x, line.res = 100, pch=1, legend=TRUE, leg.x=NA,
                  leg.y=NA, leg.cex=1, bar.length=0.1, bar.col="gray",
                  bar.lty=par("lty"), xlab='Distance', ylab='Semivariance',
                  x.line=3, y.line=3, ncol=1, main=NULL,
                  leg.label = expression(italic('n')*' size'), ...)
}
\arguments{
  \item{x}{
    'gv' object as given by 'gen.variogram'.
}
  \item{line.res}{
    Number of points in the model line.
}
  \item{pch}{
    Symbol to be used in the plot.
}
  \item{legend}{
    Boolean indicating if a legend showing \emph{n} size should be printed.
}
  \item{leg.x}{
    The x position for the legend. The legend will be placed at the right
    side of the plot if this value is set to NA.
}
  \item{leg.y}{
    The y position for the legned. The legend will be placed at the bottom
    of the plot if this value is set to NA.
}
  \item{leg.cex}{
    Multiplication factor for the legend symbol size.
}
  \item{bar.length}{
    If multiple trees are given, confidence interval bars are ploted. The
    horizontal length of the line at both bar tips is defined with this
    parameter (defaults to 0.1).
}
  \item{bar.col}{
    The color of the bars when multiple trees are given.
}
  \item{bar.lty}{
    The line type for the bars when multiple tree are given.
}
  \item{xlab}{
    The label for x axis.
}
  \item{ylab}{
    The label for y axis.
}
  \item{x.line}{
    Position of x label in lines.
}
  \item{y.line}{
    Position of y label in lines.
}
  \item{ncol}{
    Number of legend columns.
}
  \item{main}{
    Main title of the plot.
}
  \item{leg.label}{
    Legend title.
}
\item{\dots}{
  Further plotting arguments to be passed.
}
}
\details{
    Simple plot of the semi-variogram contained in a 'gv' object. If the object
    has a model, the model line is also plotted.
}
\value{
    Plot.
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

#plot semi-variogram
plot(gv)

# plot semi-variogram with model
gv <- gv.model(gv)
plot(gv)
}
\keyword{ variogram }
\keyword{ kriging }
