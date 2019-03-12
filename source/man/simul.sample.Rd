\name{simul.sample}
\alias{simul.sample}
\docType{data}
\title{
    Random samples from simulation.
}
\description{
    A table with x and y coordinates of 200 random samples in the simulated grid
    and a lineage information.
}
\usage{data(simulations)}
\format{
    'simul.sample' is a data frame with 200 rows and 3 columns. Each row is a
    sample in the simulated grid. The column one and two are the x and y
    coordinates of each sample, respectively. The lineage to which each sample
    belongs is given in the third column.
}
\references{
    Tarroso P. Carvalho S. B. and Velo-Anton G. (2018) Phylin v2: improving the phylogenetic lineage interpolation method including uncertainty and user-defined distance metrics. Submitted.
}
\examples{
data(simulations)
# Plot all the samples with different symbols for each lineage.
plot(simul.sample[,1:2], pch=simul.sample[,3])
}
\keyword{datasets}
