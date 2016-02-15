geo.dist <- function(from, to) {
    dst <- matrix(NA, nrow=nrow(from), ncol=nrow(to))
    dimnames(dst) <- list(rownames(from), rownames(to))
    for (i in 1:nrow(from))
        dst[i,] <- ((to[,1]-from[i,1])^2+(to[,2]-from[i,2])^2)^0.5
    return(dst)        
}
