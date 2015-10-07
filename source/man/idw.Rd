\name{idw}
\alias{idw}
\title{
    Inverse Distance Weighting interpolation
}
\description{
    This function interpolates a list of samples with location and a value to
    a table of coordinates, that generally represent a spatial grid. The 
    interpolation is based on inverse distance weighting algoritm with three
    different methods available for weight calculation.
}
\usage{
idw(values, coords, grid, method = "Shepard", p = 2, R = 2, N = 15)
}
\arguments{
  \item{values}{
    A table of points to be interpolated. Table must contain x and y locations,
    and a column of values to be interpolated.
}
  \item{coords}{
    A table wit x and y coordinates of the samples.
}
  \item{grid}{
    Coordinates of locations to interpolate. It is assumed to be in the same 
    order as 'values' table.
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
    It return a vector for each row of the 'coords' table with the respective
    interpolated value.
}
\references{
    Fortin, M. -J. and Dale, M. (2006) \emph{Spatial Analysis: A guide for Ecologists}. Cambridge: Cambridge University Press.

    Isaaks, E. H. and Srivastava, R. M. (1989) \emph{An Introduction to applied geostatistics}. New York: Oxford University Press.

    Legendre, P. and Legendre, L. (1998) \emph{Numerical ecology}. 2nd english edition. Amesterdam: Elsevier

    Vandergast, A. G.,Hathaway, S. A., Fisher, R. N., Boys, J., Bohonak, A. J., 
    (2008) Are hotspots evolutionary potential adequately protected in 
    southern California? \emph{Biological Conservation}, \bold{141}, 1648-1664.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\seealso{
    \code{\link{intgen.idw}}
}
\examples{
data(vipers)
data(d.gen)
data(grid)

# interpolate and plot the genetic distances for sample s2 in the d.gen
int <- idw(d.gen[,2], vipers[,1:2], grid)

grid.image(int, grid, main='IDW interpolation', xlab='Longitude', 
           ylab='Latitude', sclab="Genetic distance to sample s2")

points(vipers[,1:2], cex=d.gen[,2]*15+0.2)

# change idw power (i.e. points will have a larger influence in the 
# surroundings)
int <- idw(d.gen[,2], vipers[,1:2], grid, p=5)

result <- data.frame(grid, int)
grid.image(int, grid, main='IDW interpolation', xlab='Longitude', 
           ylab='Latitude', sclab="Genetic distance to sample s2")

points(vipers[,1:2], cex=d.gen[,2]*15+0.2)


# change idw method to "Modified Shepard" and define a maximum 
# neighbour distance
int <- idw(d.gen[,2], vipers[,1:2], grid, 'Modified', R=10)

grid.image(int, grid, main='IDW interpolation', xlab='Longitude', 
           ylab='Latitude', sclab="Genetic distance to sample s2")

points(vipers[,1:2], cex=d.gen[,2]*15+0.2)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#        Example following methods in Vandergast et al. 2008        #
#            Fit a linear model and recover the residuals           #
# ATENTION:                                                         #
#    1- Vandergast et al. (2008) suggests a RMA instead of a        # 
#       ordinary linear regression as in this example. Try package  # 
#       'lmodel2' or or other similar for RMA linear regression.    #
#    2- This example tests if the package 'geometry' is installed   #
#       to compute midpoints. If TRUE, a Delaunay triangulation is  # 
#       used, similarly to Vandergast et al. (2008). Otherwise,     #
#       midpoints are computed for the combination of all pairs of  #
#       samples.                                                    #
#                                                                   #
# the d.gen and d.real matrices in this example have the same       # 
# column and row order!                                             #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

if (is.element('geometry', installed.packages()[,1])) 
    all=FALSE else 
    all=TRUE

mp <- midpoints(vipers[,1:2], all=all)
d.real <- as.matrix(dist(vipers[,1:2]))

fit <- lm(as.vector(d.gen) ~ as.vector(d.real))
resid <- matrix(fit$residuals, nrow(vipers), nrow(vipers))
dimnames(resid) <- dimnames(d.gen)
mp$z <- extract.val(resid, mp[,1:2])

int <- idw(mp[,5], mp[,3:4], grid)

grid.image(int, grid, main='IDW interpolation', 
           xlab='Longitude', ylab='Latitude', 
           sclab="Residuals of genetic vs. real distances")

# plot samples connecting lines
for (i in 1:nrow(mp))
{
    pair <- as.character(unlist(mp[i,1:2]))
    x <- c(vipers[pair[1],1], vipers[pair[2],1])
    y <- c(vipers[pair[1],2], vipers[pair[2],2])
    lines(x, y, lty=2)
}
points(vipers[,1:2], pch=16) # plot samples points in black
points(mp[,3:4], pch=16, col='gray') # plot midpoints in gray

}
\keyword{ idw }
\keyword{ interpolation }
