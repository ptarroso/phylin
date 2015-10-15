\name{gen.variogram}
\alias{gen.variogram}
\title{
  Semi-variogram with the genetic distance matrix
}
\description{
  Computes the semi-variance with the real and genetic distances, and
  with user defined lag parameters.
}
\usage{
gen.variogram(x, y, lag = mean(x)/sqrt(nrow(x)), tol=lag/2, lmax = NA,
              bootstraps = 999, verbose = FALSE)
}
\arguments{
  \item{x}{
    Real distances matrix.
  }
  \item{y}{
    Single genetic distances matrix or list of genetic distances
    matrices.
  }
  \item{lag}{
    Real distance corresponding to the desired 'lag' interval. This is
    used to calculate lag centers from 0 to 'lmax'.
  }
  \item{tol}{
    Tolerance for the lag center to search for pairs ('lag'-'tol', 
    'lag'+'tol'). 
  }
  \item{lmax}{
    Maximum distance for lag centers. Pairs with distances higher than
    'lmax' are not included in the calcualtion of the semi-variance. If
    'lmax' is NA (default) then is used the maximum distance between
    samples.
  }
  \item{bootstraps}{
    This is the number of bootstraps used to calculate 95\% confidence
    interval for the median, when multiple genetic distances are given.
    With a single genetic distance, this parameter is ignored.
  }
  \item{verbose}{
    Boolean for verbosity. When TRUE and with multiple genetic distance
    matrices, a log of error evolution is printed.
  }
}
\details{
  This function produces a table with real lag centers and
  semi-variance. The formula to calculate semi-variance,
  \eqn{\gamma(h)}, is:

        \deqn{\gamma(h) = {\frac{1}{2 n(h)}} \sum_{i=1}^{n}[z(x_i + h) - 
        z(x_i)]^2}{y(h) = 1/(2*n(h)) * sum((z(xi+h) - z(xi))**2)}

   where \eqn{n(h)} is the number of pairs with the lag distance \eqn{h} 
   between them, and \eqn{z} is the value of the sample \eqn{x} at the
   the location \eqn{i}. The difference between sample
   \eqn{z(x_i+h)}{x(xi+h)} and sample \eqn{z(x_i)}{x(xi)} is assumed to
   correspond to their genetic distance.

   Multiple genetic distance matrices can be used. In this case, a
   variogram is computed for each genetic distance and the results
   summarised by the median and a 95\% confidence interval calculated
   with bootstraps. 
}
\value{
    Returns a 'gv' object with the input data, lag centers and semi-variance.
}
\references{
    Fortin, M. -J. and Dale, M. (2006) \emph{Spatial Analysis: A guide for Ecologists}. Cambridge: Cambridge University Press.

    Isaaks, E. H. and Srivastava, R. M. (1989) \emph{An Introduction to applied geostatistics}. New York: Oxford University Press.

    Legendre, P. and Legendre, L. (1998) \emph{Numerical ecology}. 2nd english edition. Amesterdam: Elsevier
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\note{
    It is assumed that the order of samples in x corresponds to the same in y.
}

\seealso{
    \code{\link{plot.gv}}
    \code{\link{predict.gv}}
    \code{\link{gv.model}}
}
\examples{

    data(vipers)
    data(d.gen)

    # create a distance matrix between samples
    r.dist <- dist(vipers[,1:2])

    # variogram table with semi-variance and lag centers
    gv <- gen.variogram(r.dist, d.gen)

    # plot variogram
    plot(gv)

    # fit a new variogram with different lag
    gv2 <- gen.variogram(r.dist, d.gen, lag=0.2)
    plot(gv2)

}
\keyword{ variogram }
\keyword{ kriging }
