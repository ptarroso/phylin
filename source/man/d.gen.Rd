\name{d.gen}
\alias{d.gen}
\docType{data}
\title{
    Genetic distance matrix between vipers and lineages.
}
\description{
    This is a matrix of genetic distances between the \emph{Vipera latastei}
    samples. The values are cophenetic distances generated from the 
    phylogenetic tree.
}
\usage{data(d.gen)}
\format{
    'd.gen' is a matrix with 58 rows and columns. Columns and rows are 
    organized with the same order found in the 'vipers' dataset.
}
\references{
    Velo-Anton G., Godinho R., Harris D. J. \emph{et al.} (2012) Deep evolutionary lineages in a Western Mediterranean snake (\emph{Vipera latastei}/\emph{monticola} group) and high genetic structuring in Southern Iberian populations. \emph{Molecular phylogenetics and evolution}, \bold{65}, 965--973.
}
\examples{
data(d.gen)
hc <- hclust(as.dist(d.gen))
plot(hc, hang = -1, main="Vipers genetic distance tree", 
     xlab="Samples", cex=0.7)
}
\keyword{datasets}
