\name{vipers}
\alias{vipers}
\docType{data}
\title{
    Vipers sample locations for 'd.gen' dataset.
}
\description{
    This dataset contains the x and y coordinates of 58 \emph{Vipera latastei}
    samples with corresponding lineages.
}
\usage{data(vipers)}
\format{
    A data frame with 3 columns (x/Longitude, y/Latitude and lineage) and 58
    rows. 
}
\source{
    Velo-Anton G., Godinho R., Harris D. J. \emph{et al.} (2012) Deep evolutionary lineages in a Western Mediterranean snake (\emph{Vipera latastei}/\emph{monticola} group) and high genetic structuring in Southern Iberian populations. \emph{Molecular phylogenetics and evolution}, \bold{65}, 965--973.
}
\examples{
data(vipers)
data(grid)
plot(grid, cex=0.5, col='lightgrey', asp=1,
     main="Vipers data", xlab="Longitude", ylab="Latitude")
points(vipers[,1:2], pch=vipers$lin)
legend(1, 38, legend=c("West", "South", "East"), pch=1:3, title="Lineages")

}
\keyword{datasets}
