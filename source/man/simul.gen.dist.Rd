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
    Tarroso P. Carvalho S. B. and Velo-Anton G. (2018) Phylin v2: improving the phylogenetic lineage interpolation method including uncertainty and user-defined distance metrics. Submitted.
}
\examples{
data(simulations)
hc <- hclust(as.dist(simul.gen.dist))
plot(hc, main="Simulated gentic distances", xlab="Samples", cex=0.7)
}
\keyword{datasets}
