#' Random normal sample from a chain graph
#' 
#' Generates a desired number of normal random samples from a given chain graph
#' structure and a given block-recursive regression system compatible with the
#' chain graph.
#' 
#' The function uses a mean \eqn{0} block-recursive regression model (See
#' Wermuth (1992)), which is recorded in \code{Bstar} and normal random samples
#' are generated from the specified block-recursive regression model.
#' 
#' @usage rnorm.cg(n, amat, Bstar)
#' @param n the intended sample size, should be at least \eqn{1}.
#' @param amat the adjacency matrix of a chain graph.
#' @param Bstar the matrix representation of the block-recursive regression
#' structure, e.g., the one returned by \code{\link{get.normal.dist}}.
#' @return An \eqn{n} by \eqn{p} matrix with each row corresponding to a random
#' sample.
#' @author Zongming Ma and Xiangrui Meng
#' @references Wermuth, N. (1992). Block-recursive regression equations (with
#' discussions). \emph{Revista Brasileira de Probabilidade e Estatistica,}
#' \bold{6}, 1-56.
#' @keywords multivariate datagen distribution
`rnorm.cg` <-
function(n, amat, Bstar)
{
    fcheck <- is.chaingraph(amat)
    if(!fcheck$result)
        stop("The input should be a chain graph!")
    p <- nrow(amat)
    corder <- rev(fcheck$vert.order)
    csize <- rev(fcheck$chain.size)
    diag(amat) <- 1
    if(!all(rownames(Bstar) == rownames(amat)[corder]) ||
       !all((t(Bstar) == 0) == (amat[corder, corder] == 0)))
        stop("The Bstar and amat are not compatible!")
    b <- cumsum(csize)
    W <- matrix(nrow = p, ncol = n)
    for(k in 1:length(csize)){
        idx <- (max(b[k-1],0)+1):b[k]
        if(csize[k] > 1)
            W[idx, ] <- t(mvrnorm(n=n, mu=rep(0,csize[k]), Sigma=Bstar[idx, idx]))
        if(csize[k] == 1)
            W[idx, ] <- rnorm(n=n, mean=0, sd=sqrt(Bstar[idx, idx]))
    }
    t(solve(Bstar, W))
}

