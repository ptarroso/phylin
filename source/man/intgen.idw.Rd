\name{intgen.idw}
\alias{intgen.idw}
\title{
    Interpolation of genetic distances to a a grid of points.
}
\description{
    Interpolations of a matrix containing genetic distances using the Inverse
    Distance Weighting (IDW) algorithm. It generates a matrix of interpolated
    values for each grid cell and for each sample.
}
\usage{
intgen.idw(d.real, d.gen, method = "Shepard", p = 2, R = 2, N = 15)
}
\arguments{
  \item{d.real}{
    distance matrix between sampled locals (columns) and locals where 
    interpolation is to be executed (rows). Names should correspond to genetic
    distances matrix.
}
  \item{d.gen}{
    genetic distances matrix. Names should correspond to real distances matrix,
    but not necessarily in the same order.
}
  \item{method}{
    Method to calculate weights for idw. Should be "Shepard" (default), 
    "Modified", "Neighbours", or distinctive abreviations of each. See details
    section for additional help on each method.
}
  \item{p}{
    The power to use in weight calculation.
}
  \item{R}{
    Radius to use with Modified Shepard method.
}
  \item{N}{
    Maximum number of neighbours to use with Shepard with neighbours.
}
}
\details{
    The IDW interpolation algorithm is commonly used to interpolate genetic
    data over a spatial grid. This function provides a simple interface to
    interpolate such data with three methods:
    \enumerate{
    \item{\emph{Shepard}: 
          weights are the inverse of the distance between the interpolation
          location \eqn{x} and the sample points \eqn{x_i}, raised to the 
          power \eqn{p}
          \deqn{w(x) = \frac{1}{d(x, x_i)^p}}{w(x) = 1/d(x, xi)^p}
    }
    \item{\emph{Modified Shepard}:
        distances are weighted with a search radius \eqn{r} to calculate the 
        interpolation weights 
        \deqn{w(x) = \left(\frac{r-d(x, x_i)}{r.d(x, xi)}\right)^p}{
              w(x) = ((r-d(x, xi)) / (r*d(x, xi)))^p}
    }
    \item{\emph{Shepard with neighbours}:
        A maximum ammount of \eqn{N} neighbours is allowed to the weight 
        calculation following Shepard method.
    }
    }
}
\value{
    This function returns a matrix containing all interpolated values for each
    locality (rows) and for each sample (columns)
}
\references{
    Fortin, M. -J. and Dale, M. (2006) \emph{Spatial Analysis: A guide for Ecologists}. Cambridge: Cambridge University Press.

    Isaaks, E. H. and Srivastava, R. M. (1989) \emph{An Introduction to applied geostatistics}. New York: Oxford University Press.

    Legendre, P. and Legendre, L. (1998) \emph{Numerical ecology}. 2nd english edition. Amesterdam: Elsevier
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\seealso{
    \code{\link{idw}}
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

    #plot the 12 random interpolations
    layout(matrix(c(1:12), 4,3))

    for (i in sample(1:nrow(vipers), 12))
    {
        dt <- data.frame(grid, int=result[,i])
        # when samples are given with real coordinates, aspect of image 
        # should be maintained with asp=1 to plot properly. 
        image(sort(unique(grid[,1])), sort(unique(grid[,2])), 
              xtabs(int~x+y, dt), xlab='Longitude', ylab='Latitude', 
              main=colnames(result)[i])
        cex <- (d.gen[,i]-min(d.gen[,i]))/(max(d.gen[,i])-min(d.gen[,i]))
        points(vipers[,1:2], cex=cex+0.5)
    }
}
\keyword{ idw }
\keyword{ interpolation }
