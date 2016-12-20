\name{gv.model}
\alias{gv.model}
\title{
    Fit a model to the semi-variogram.
}
\description{
    Fits a model to a semi-variogram contructed with
    \code{\link{gen.variogram}}. Parameters for each model are estimated by
    nonlinear least squares.
}
\usage{
gv.model(gv, model='spherical', sill = NA, range=NA, nugget = 0,
         ctrl=nls.control())
}
\arguments{
  \item{gv}{
    'gv' object from the \code{\link{gen.variogram}} function.
}
  \item{model}{
    Model to fit the data. Available models are spherical (default),
    gaussian, exponential or linear. See details.
}
  \item{sill}{
    The heigth (semi-variance) of the model when it stabilizes. Defaults to
    NA.
}
  \item{range}{
    The length (real distance) where stabilization occurs. Defaults to NA.
}
  \item{nugget}{
    Intercept in the y-axis. Defaults to 0.
}
  \item{ctrl}{
    Nls control object for the fitting procedure. (check ?nls.control for more
    details).
}
}
\details{
    This function fits a model to the data, either by estimating the model
    parameters using nonlinear least squares or by user provided values.
    Notice that parameters with NA will be estimated by the nls function
    and parameters with values given are not estimated (by default, the
    nugget is not estimated and set to zero).
    The variogram model can be plotted using 'plot' function or used to predict
    to a new set of values using 'predict'. It is used to define weights for
    the krigging interpolation.

    The parameters of the semi-variogram model (\eqn{\gamma}{y}) are the
    distance (\eqn{h}), range (\eqn{a}), sill (\eqn{c = c_0 + c_1};
    where \eqn{c_1} is the partial sill), and nugget
    (\eqn{c_0}{c0}).
    The models available are:

    \enumerate{

        \item{gaussian:
              \deqn{\gamma(h) = c_0 + c_1 \left(1-exp\left(-3\frac{h^2}
              {a^2}\right)\right)}{y(h) = c0 + c1*(1-exp(-3*h**2/a**2))}}
        \item{exponential:
              \deqn{\gamma(h) = c_0 + c_1 \left(1-exp\left(-\frac{3h}{a}
              \right)\right)}{y(h) = c0 + c1 * (1-exp(-3h/a))}}
        \item{spherical:
              \deqn{ \gamma(h) = \left\{
                \begin{array}{ll}
                c_0 + c_1\left(\frac{3}{2}\frac{h}{a} -
		                   \frac{1}{2}(\frac{h}{a})^3\right) &
                    0 < h \le a \\
                    c & h > a
                    \end{array}\right. }{y(h) = c0 + c1*((3/2)(h/a)-
		    (1/2)(h/a)**3), for 0 < h <= a; y(h) = c, for h > a )}}
        \item{pentaspherical:
              \deqn{ \gamma(h) = \left\{
                \begin{array}{ll}
                c_0 + c_1\left(\frac{15}{8}\frac{h}{a} -
		                   \frac{5}{4}(\frac{h}{a})^3 +
		                   \frac{3}{8}(\frac{h}{a})^5\right) &
                    0 < h \le a \\
                    c & h > a
                    \end{array}\right. }{y(h) = c0 + c1*((15/8)(h/a)-(5/4)(h/a)**3+(3/8)(h/a)**5), for 0 < h <= a; y(h) = c, for h > a )}}
        \item{cubic:
              \deqn{ \gamma(h) = \left\{
                \begin{array}{ll}
                c_0 + c_1\left(7(\frac{h}{a})^2 -
		                \frac{35}{4}(\frac{h}{a})^3 +
				\frac{7}{2}(\frac{h}{a})^5 -
				\frac{3}{4}(\frac{h}{a})^7\right) &
                    0 < h \le a \\
                    c & h > a
                    \end{array}\right. }{y(h) = c0 + c1*(7(h/a)**2-(35/4)(h/a)**3+(7/2)(h/a)**5-(3/4)(h/a)**7), for 0 < h <= a; y(h) = c, for h > a )}}

	\item{linear:
              \deqn{ \gamma(h) = \left\{
                \begin{array}{ll}
                c_0 + \left(\frac{c_1}{a}h\right) &
                    0 < h \le a \\
                    c & h > a
                    \end{array}\right. }{y(h) = c0 + (c1/a)h, for 0 < h <= a; y(h) = c, for h > a )}}
    }
}
\value{
    Returns a 'gv' object with the model, input data, and parameter values.
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
    \code{\link{plot.gv}}
    \code{\link{predict.gv}}
}
\examples{

data(vipers)
data(d.gen)

# create a distance matrix between samples
r.dist <- dist(vipers[,1:2])

# fit a variogram with defaults (shperical model) and estimation of range
gv <- gen.variogram(r.dist, d.gen)
gv <- gv.model(gv)

# plot variogram
plot(gv)

# fit a new variogram with linear with sill model and range 8
gv2 <- gv.model(gv, model='linear', range=8)
plot(gv2)

}
\keyword{ variogram }
\keyword{ kriging }
