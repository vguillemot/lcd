#' Random multinomial sample from a chain graph
#' 
#' Generates a desired number of multinomial random samples from a given chain
#' graph structure and a sequence of compatible block-recursive conditional
#' distributions.
#' 
#' 
#' @usage rmultinom.cg(n, amat, distn)
#' @param n the intended sample size, should be at least \eqn{1}.
#' @param amat the adjacency matrix of a chain graph.
#' @param distn a sequence of block-recursive conditional distributions, e.g.
#' the one returned by \code{\link{get.multinom.dist}}.
#' @return An \eqn{n} by \eqn{p} matrix with each row corresponding to a random
#' sample.
#' @author Zongming Ma and Xiangrui Meng
#' @references Cowell, R. G., Dawid, A. P., Lauritzen, S. L. and Spiegelhalter,
#' D. J. (1999) \emph{Probabilistic Networks and Expert Systems}. pp.77-79.
#' Springer-Verlag, New York.
#' @keywords multivariate datagen distribution
`rmultinom.cg` <-
    function(n, amat, distn)
{
    vset <- rownames(amat)
    p <- length(vset)
    data <- matrix(NA, n, p)
    colnames(data) <- vset
    n.c <- length(distn)
    for(i in 1:n.c){
        temp <- distn[[i]]
        V.idx <- match(colnames(temp$Vtable), vset)
        if(ncol(temp$cond.dist) > 1){
            B.idx <- match(colnames(temp$Btable), vset)
            if (length(B.idx) > 1) {
              config <- apply(as.matrix(data[,B.idx]), 1, function(x)
                              which(apply(temp$Btable, 1, function(y) identical(x,y))))
            } else {
              config <- apply(as.matrix(data[,B.idx]), 1, function(x)
                              which(apply(temp$Btable, 1, function(y) y==x)))
            }
            row <- sapply(config, function(x)
                          sample(nrow(temp$Vtable), 1, prob = temp$cond.dist[,x]))
            data[,V.idx] <- temp$Vtable[row,]
        } else {
            data[,V.idx] <- temp$Vtable[sample(nrow(temp$Vtable), n, TRUE,
                                              prob = temp$cond.dist[,1]),]
        }
    }
    data
}

