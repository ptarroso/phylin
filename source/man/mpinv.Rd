\name{mpinv}
\alias{mpinv}
\title{
    Generalized inverse of a matrix
}
\description{
    Computes the generalized inverse of a matrix using singular-value
    decomposition.
}
\usage{
mpinv(A, eps = 1e-13)
}
\arguments{
  \item{A}{
    Matrix to be inverted.
}
  \item{eps}{
    Minimum value threshold.
}
}
\value{
    Returns a matrix containing the inverse of matrix A.
}
\author{
    Pedro Tarroso <ptarroso@cibio.up.pt>
}
\examples{
    m <- matrix(rnorm(16), 4, 4)
    mi <- mpinv(m)
}
\keyword{ inverse }
\keyword{ matrix }
