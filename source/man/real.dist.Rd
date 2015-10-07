\name{real.dist}
\alias{real.dist}
\title{
    Real distance matrix for samples and interpolation locations.
}
\description{
    Calculates a real euclidean distances matrix based on a list of coordinates
    for samples and for interpolation locations.
}
\usage{
real.dist(samples, grid, samples.names = rownames(samples), grid.names = rownames(grid))
}
\arguments{
  \item{samples}{
    Table of coordinates for sampled locations. Should have two columns 
    (longitude and latitude) in the same order as grid.
}
  \item{grid}{
    Table of locations to where distances to samples are calculated. Should
    have two columns (longitude and latitude) in the same order as samples.
}
  \item{samples.names}{
    Vector of sample names to attribute to final matrix columns. Defaults to
    row names of 'samples' table.
}
  \item{grid.names}{
    Vector of grid names (ID) to attribute to final matrix rows. Defaults to
    row names of 'grid' table.
}
}
\value{
    Return the matrix of euclidean distances between sampled locations are 
    those to get interpolated values.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\examples{
data(vipers)

# create a grid of the sampled area
grid <- expand.grid(x=seq(-10,10,0.5), y=seq(30, 50, 0.5))

rd <- real.dist(vipers[,1:2], grid)
}
\keyword{ distance }
