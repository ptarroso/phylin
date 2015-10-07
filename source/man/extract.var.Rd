\name{extract.val}
\alias{extract.val}
\title{
    Extact pairwise values from a matrix in a specified order.
}
\description{
    This function extacts pairwise values from a matrix in a specified 
    order in a user provided table.
}
\usage{
extract.val(m, samples) 
}
\arguments{
  \item{m}{
    Matrix with values to extract.
}
  \item{samples}{
    Data frame with columns indicating pairs of samples to extract values.
    Names must correspond to column and row names in the matrix.
}
}
\details{
    This function extracts the values from a matrix in the same pairs of 
    populations/samples given in a table. It is useful for merging data
    from a distance matrix of samples and the midpoints between samples (in
    conjuction with \code{\link{midpoints}} function).
}
\value{
    Returns a vector containing the values from the matrix m in the order given
    in samples. 
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\seealso{
    \code{\link{dist}}
    \code{\link{d.gen}}
    \code{\link{midpoints}}
    \code{\link{idw}}
}
\examples{

data(vipers)
data(d.gen)

# calculate midpoints
mp <- midpoints(vipers[,1:2])

# extract values from d.gen. Columns 1 and 2 of mp have the information 
# about source and target samples.   
pair.data <- extract.val(d.gen, mp[,1:2]) 

# it is easier to view in a plot:
plot(vipers[,1:2], pch=vipers[,3], main="Midpoints between samples", 
     xlab="Longitude", ylab="Latitude")
#trace all connecting lines between samples
sps <- rownames(vipers)
for (i in 1:nrow(mp)) 
{
    sp <- mp[i, 1:2] #source an target samples
    mask <- c(which(sps == sp[,1]), which(sps == sp[,2]))
    lines(vipers$x[mask], vipers$y[mask], lty=2, col='lightgrey')
}

#midpoints with genetic distance acentuated
points(mp[,3:4], col='red', pch=16, cex=pair.data*15+0.5) 

}
\keyword{ variogram }
\keyword{ kriging }
