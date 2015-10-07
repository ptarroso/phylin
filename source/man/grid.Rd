\name{grid}
\alias{grid}
\docType{data}
\title{
    Grid centroids for the Iberian Peninsula.
}
\description{
    This is a list of coordinates representing the interpolation area in 
    the Iberian Peninsula with a resolution of 0.09 degrees (~10km). 
}
\usage{data(grid)}
\format{
    The data format is a table with two columns (longitude, and latitude) 
    and 7955 rows (pixels).
}
\examples{
data(grid)
plot(grid, cex=0.5, asp=1, main="Grid of pixels for interpolation",
     xlab="Longitude", ylab="Latitude")
}
\keyword{datasets}
