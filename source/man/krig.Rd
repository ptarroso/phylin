\name{krig}
\alias{krig}
\title{
    Simple and ordinary kriging.
}
\description{
    Computes simple or ordinary kringing using a list of sampled locations. 
    The interpolation is executed to the table of coordinates given. 
}
\usage{
krig(values, coords, grid, gv, m=NA, cv=FALSE, clamp = FALSE, verbose=TRUE)
}
\arguments{
  \item{values}{
    A vector of values per sampled location.
}
  \item{coords}{
    A table containing longitude and latitude of sample locations for each
    value.
}
  \item{grid}{
    A table containing the coordinates of locations to interpolate
}
  \item{gv}{
    A fitted model to variogram as given by 'gv.model' function.
}
  \item{m}{
    A value for the mean. When the mean is known and given, a simple kriging 
    is used for the interpolation. If m = NA (default) then the mean is 
    estimated using ordinary kriging.
}
  \item{cv}{
    A logical value to perform cross validation of the interpolation.
}
\item{clamp}{
    A logical value indicating if Z values will be adjusted to the interval 
    [0,1].
}
  \item{verbose}{
    A logical indicating if the function should be verbose.
}
}
\details{
    This function interpolates the probability of lineage occurrence to all
    locations given in 'coords'. Usually 'coords' stores coordinates of 
    pixel centroids representing the study area with a user-defined spatial
    resolution. The variogram with a fitted model describes the 
    autocorrelation structure of the genetic data. This is used by kriging
    to determine the weight of the sampled points on the location to 
    predict a value.

    Cross-validation is computed by leaving each of the observation in 'values' 
    out of kriging and predict for the same location. A mean squared error 
    (MSE) is computed using the original observation and the predicted value.
}
\value{
    Returns a vector of interpolated values and respective variance for each 
    location in 'coords'.
    
    If cross-validation is performed (cv=TRUE) than a list of interpolation
    and variance values is given with a cross-validation matrix (original
    observation and predicted value) and a mean squared error (MSE).
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
    \code{gen.variogram}
    \code{plot.gv}
    \code{predict.gv}
    \code{idw}
    \code{intgen.idw}
}
\examples{
data(vipers)
data(d.gen)
data(grid)

# In this example we want to create the probable distribution of a 
# lineage based on the genetic distance. We need a vector defining if
# each sample belongs or not to the lineage
lin <- as.integer(vipers$lin == 1)

# create a distance matrix between samples
r.dist <- dist(vipers[,1:2])

# fit a model with defaults (spherical model) and estimation of range
gv <- gen.variogram(r.dist, d.gen)
gv <- gv.model(gv)

# perform interpolation with ordinary kriging
int.krig <- krig(lin, vipers[,1:2], grid, gv)

#plot the interpolation results
grid.image(int.krig, grid, main='Kriging with genetic distances',
           xlab='Longitude', ylab='Latitude', 
           sclab='Lineage interpolation')
points(vipers[,1:2], pch=lin+1)

# plot the interpolation standard variance
grid.image(int.krig, grid, ic='sd', 
           main='Kriging with genetic distances',
           xlab='Longitude', ylab='Latitude', 
           sclab='Standard deviation')
points(vipers[,1:2], pch=lin+1)

#plot only pixels higher than 0.95
lin.krig <- as.integer(int.krig$Z>0.95)
grid.image(lin.krig, grid, main='Kriging with genetic distances',
           xlab='Longitude', ylab='Latitude', sclab='Lineage')
points(vipers[,1:2], pch=lin+1)

\dontrun{
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# The following code is an example to combine the possible clusters   #
#  of a tree in a single map. It samples the tree at different length #
# and combines the probabilities.                                     #
#                                                                     #
#                NOTE: it may take some time to run!                  #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# A phylogenetic tree is sampled at different lengths. An appropriate  
# package (e.g. ape) should be used to process the tree. To avoid extra
# dependencies we convert the genetic distances to an hclust.

hc <- hclust(as.dist(d.gen))
hs = seq(0.01, 0.08, 0.005) # tree length sampling

# Another options for tree sampling can be using the nodes position, 
# avoiding the root and tip levels:
# hs <- hc$height[hc$height > 0.0 & hc$height < max(hc$height)]
#
# Or a single threshold:
# hs <- 0.06

contact = rep(0, nrow(grid)) # Sums all probabilities
for (h in hs) {
    lins <- cutree(hc, h=h)
    print(paste("height =", h, ":", max(lins), "lineages")) #keep track
    ct = rep(1, nrow(grid)) # Product of individual cluster/lineage map
    for (i in unique(lins)) {
        lin <- as.integer(lins == i)
        krg <- krig(lin, vipers[,1:2], grid, gv, 
                    clamp = TRUE, verbose=FALSE)
        # Probability of NOT belonging to a cluster.
        ct <- ct * (1 - krg$Z) # Probab. of NOT belonging to a cluster
    }
    contact = contact + ct
}
krg$Z <- contact / length(hs) # Recycle krg with contact zones

#plot the interpolation results
grid.image(krg, grid, xlab='Longitude', ylab='Latitude', 
           main='Uncertainty in cluster classification / contact zones')
points(vipers[,1:2], pch=16, cex=0.5)
}
}
\keyword{ interpolation }
\keyword{ kriging }
