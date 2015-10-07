\name{midpoints}
\alias{midpoints}
\title{
    Midpoints between pairs of coordinates
}
\description{
    Computes the midpoints for a table of sample points with coordinates.
}
\usage{
midpoints(samples, x=1, y=2, sp.name=row.names(samples), all=FALSE)
}
\arguments{
  \item{samples}{
    Table with coordinates for each sample point.
}
  \item{x}{
    Column index or name of longitudes (x) in samples table (default is first column).
}
  \item{y}{
    Column index or name of latitudes (y) in samples table (default is second column).
}
  \item{sp.name}{
    Name for each sample point (defaults to row names of samples).
}
  \item{all}{
    If TRUE computes midpoints between all sample points. If FALSE (default) 
    computes a Delaunay triangulation and the midpoints of the resulting
    connected samples.
}
}
\details{
    This function computes the coordinates of the middle points between 
    samples. The connecting network can be between all points or between 
    neighbours with non-overlaping edges after a Delaunay triangulation.
}
\value{
    Returns a data frame with 4 columns referring the source and target
    samples (ss and ts, respectively) and the coordinates of the midpoints.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\note{
    Depends on package 'geometry' for Delaunay triangulation.
}

\seealso{
    \code{\link{dist}}
    \code{\link{d.gen}}
    \code{\link{extract.val}}
}
\examples{

data(vipers)
mp <- midpoints(vipers[,1:2], all=TRUE) 
# With 'all=FALSE' (Delaunay triang.), package 'geometry' is mandatory.

}
\keyword{ variogram }
\keyword{ kriging }
