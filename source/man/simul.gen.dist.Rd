\name{simul.gen.dist}
\alias{simul.gen.dist}
\docType{data}
\title{
    Simulated genetic distances.
}
\description{
    A 'dist' class object with simulated genetic distances.
}
\usage{data(simulations)}
\format{
    'simul.gen.dist' is an object of 'dist' class containing the simulated
    genetic distances of 200 samples.
}
\references{
    Tarroso, P., Carvalho, S.B. & Velo-Anton, G. (2019). PHYLIN 2.0: Extending the phylogeographic interpolation method to include uncertainty and user-defined distance metrics. Molecular Ecology Resources, in press.
}
\examples{
data(simulations)
hc <- hclust(as.dist(simul.gen.dist))
plot(hc, main="Simulated gentic distances", xlab="Samples", cex=0.7)
}
\keyword{datasets}
