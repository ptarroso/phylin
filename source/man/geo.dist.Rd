\name{geo.dist}
\alias{geo.dist}
\title{
    Geographical distance matrix for samples and interpolation locations.
}
\description{
  Calculates a geographical euclidean distances matrix based on a list
  of coordinates for samples and for interpolation locations.
}
\usage{
geo.dist(from, to)
}
\arguments{
  \item{from}{
    Data frame with coordinates for source locations. Should have two
    columns (longitude and latitude).
}
  \item{to}{
    Data frame with coordinates for destination locations to where
    distances are calculated. Should have two columns (longitude and
    latitude).
}
}
\value{
  Return the matrix of euclidean distances between source locations
  ('from') to destination ('to') coordinates. The resulting matrix has
  source locations in rows and destination in columns and respective
  names are given based on the row names of the 'from' and 'to' data
  frames.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\examples{
data(vipers)

# create a grid of the sampled area
grid <- expand.grid(x=seq(-10,10,0.5), y=seq(30, 50, 0.5))

rd <- geo.dist(vipers[,1:2], grid)
}
\keyword{ distance }
