\name{multispecies}
\alias{multispecies}
\title{
    Summarizes data from multiple species.
}
\description{
    This function may be used to summarize data from different species or 
    similar (e.g. different lineages, etc). By default, it summarizes with
    the mean and standard deviation, but different functions may be used. 
}
\usage{
multispecies(..., FUN=list(mean=mean, sd=sd), na.rm=FALSE)
}
\arguments{
  \item{...}{
    Input data to be summarized. It must be numeric and it can be multiple
    vectors of the same size and order, or more simply a matrix with all
    data to be summarised. The matrix must have the different species/data in 
    columns. 
    
}
  \item{FUN}{
    This is a list of functions to be applied to summarize the data. By default
    it uses the mean and sd, but it can be any other function that returns a
    number from a vector (e.g. max, min) or a user-defined function. If the
    objects are named in the FUN list, than those names will be given to the
    resulting columns. Otherwise, function are applied in the same order as
    given.
}
  \item{na.rm}{
    A logical indicating whether missing values should be removed. Will only
    work if the functions in FUN accept it. 
}
}
\details{

    This function is a simple wrapper with some error checking for the native
    R function 'apply'. 
}
\value{
    Returns a matrix with functions applied in the same order as FUN.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\seealso{
    \code{\link{apply}}
    \code{\link{princomp}}
    \code{\link{prcomp}}
}
\examples{
data(vipers)
data(d.gen)
data(grid)

# create a matrix of distances from sample points (columns) to all
# grid pixels
rd <- real.dist(vipers[,1:2], grid)

#interpolate with idw
result <- intgen.idw(rd, d.gen)

ms <- multispecies(result)

# plot the mean
grid.image(ms, grid, main = "Mean")

# plot the standard deviation
grid.image(ms, grid, ic=2, main = "Standard Deviation")
}
\keyword{ idw }
\keyword{ interpolation }
