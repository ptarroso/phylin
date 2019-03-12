\name{simul.env}
\alias{simul.env}
\docType{data}
\title{
    Simulated environments.
}
\description{
    A table with two gridded simulation environments.
}
\usage{data(simulations)}
\format{
    'simul.env' is a data frame with 1849 rows and 4 columns. Each row is a cell
    in the gridded surface. The column one and two are the x and y centroid
    coordinates of each cell, respectively. The third and fourth columns are the
    two simulates environments.
}
\references{
    Tarroso, P., Carvalho, S.B. & Velo-Anton, G. (2019). PHYLIN 2.0: Extending the phylogeographic interpolation method to include uncertainty and user-defined distance metrics. Molecular Ecology Resources, in press.
}
\examples{
data(simulations)
# Plot the second environmental surface: negative values as open circles and
# positive as solid black circles. Size proportional to absolute value
plot(simul.env[,1:2], pch=1, cex=simul.env[,4]/5)
points(simul.env[,1:2], pch=16, cex=-simul.env[,4]/5)
}
\keyword{datasets}
