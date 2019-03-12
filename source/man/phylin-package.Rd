\name{phylin-package}
\alias{phylin-package}
\alias{phylin}
\docType{package}
\title{
    Phylogenetic Landscape Interpolation.
}
\description{This package provides functions for the spatial interpolation
             of genetic distances between samples. The interpolation is
             based on a modified kriging method that accepts a genetic
             distance matrix and generates a map of probability of lineage
             presence. This package also offers tools to generate a map of
             potential contact zones between groups with user-defined
             thresholds in the tree to account for old and recent
             divergence. Additionally, it has functions for IDW
             interpolation using genetic data and midpoints.
}
\details{
\tabular{ll}{
Package: \tab phylin\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2016-04-27\cr
License: \tab GPL-2\cr
}
    The kriging algorithm uses a model fitted to the semi-variogram to weight
    the values of the samples. Here the variogram was modified to fit a
    model with pairwise comparison between genetic and real distances,
    describing the spatial dependence in the genetic distance between samples.
    A map for the lineage can be generated using only a vector that define if
    each point belong to the a lineage or not.

    Since version 2.0, the kriging interpolation can be performed taking
    into consideration a cost distance instead of simple geographical
    distances between points. This can help in cases where a landscape
    resistance explains better the genetic distances between samples
    than the geographic distances alone.

    The IDW can be used to interpolate the genetic distance of each sample
    against the others, or to interpolate genetic diverge at midpoints between
    samples. The interpolated value at certain location is obtained by
    weighting with the distances to the avaialable samples. Similarly to
    kriging, these distance can be based on a cost distance calculation.

}
\author{
Pedro Tarroso, Guillermo Velo-Anton, Silvia Carvalho

Maintainer: Pedro Tarroso <ptarroso@cibio.up.pt>
}
\references{
Tarroso, P., Velo-Anton, G., & Carvalho, S.B. (2015). PHYLIN: an R package for phylogeographic interpolation. Molecular Ecology Resources, 15(2), 349-357.

Tarroso, P., Carvalho, S.B. & Velo-Anton, G. (2019). PHYLIN 2.0: Extending the phylogeographic interpolation method to include uncertainty and user-defined distance metrics. Molecular Ecology Resources, in press.

}
\keyword{ package }
\examples{
    ## See examples for the included functions.
}
