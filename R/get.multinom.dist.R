#' Random multinomial distribution generation from a chain graph
#' 
#' Generates a random multinomial distribution from a given chain graph
#' structure.
#' 
#' The function returns a list that encodes a block-recursive conditional
#' distribution over the chain graph structure.
#' 
#' @usage get.multinom.dist(amat, n.state, alpha, beta)
#' @param amat the adjacency matrix of a chain graph.
#' @param n.state a vector of positive integers indicating the desired number
#' of states for each variable in the chain graph.
#' @param alpha,beta shape parameters for Beta distribution in simulating the
#' potentials.
#' @return A sequence of block-recursive conditional distributions generated
#' according to the graph structure.
#' @author Zongming Ma and Xiangrui Meng
#' @references Cowell, R. G., Dawid, A. P., Lauritzen, S. L. and Spiegelhalter,
#' D. J. (1999) \emph{Probabilistic Networks and Expert Systems}. pp.77-79.
#' Springer-Verlag, New York.
#' @keywords multivariate distribution
`get.multinom.dist` <- function(amat, n.state, alpha = 1, beta = 1)
  ## n.state is a vector of number of states for each r.v.
{
    cliq <- .lcd.cliques
    fcheck <- is.chaingraph(amat)
    if(!fcheck$result)
        stop("The input should be a chain graph!")
    p <- nrow(amat)
    vset <- rownames(amat)
    corder <- fcheck$vert.order
    csize <- fcheck$chain.size
    b <- cumsum(csize)
    n.c <- length(csize)
    distn <- vector("list", n.c)
    for(i in 1:n.c){
        V <- vset[corder[(ifelse(i==1, 0, b[i-1]) + 1):b[i]]]
        B <- pa(V, amat)
        Kstar <- as.matrix(amat[c(B,V), c(B,V)])
        if(length(B) != 0){
                                        # make B(t) complete in K*(t)
            Kstar[B,B] <- 1
            diag(Kstar) <- 0
        }
        if(nrow(Kstar) == 1) A <- list(V)
        else A <- cliq(Kstar)
        pot <- vector("list", length(A))
        for(j in 1:length(A))
                                        # specify potential for each clique in K*(t)
            pot[[j]] <- .make.table(vset, n.state, A[[j]], alpha, beta, TRUE)
        if(length(B) == 0){
            Btable <- matrix(NA,1,1)
        } else {
            Btable <- .make.table(vset, n.state, B, alpha, beta, FALSE)
        }
        Vtable <- .make.table(vset, n.state, V, alpha, beta, FALSE)
        cond.dist <- matrix(0, nrow(Vtable), nrow(Btable))
                                        # each column is a conditional distribution
        for(k in 1:nrow(Btable)) for(l in 1:nrow(Vtable)){
            config <- c(Btable[k,], Vtable[l,])
            config <- config[!is.na(config)]
            vert <- c(B,V)
            m <- 1
            for(r in 1:length(pot))
            {
                test <- config[match(A[[r]], vert)]
                row <- apply(pot[[r]], 1, function(x) all(x[-length(x)] == test))
                m <- m * pot[[r]][ ,"Freq"][row]
            }
            cond.dist[l,k] <- m
        }
        cond.dist <- apply(cond.dist, 2, function(x) x / sum(x))
        distn[[i]] <- list(Btable=Btable,
                           Vtable=Vtable,
                           cond.dist=cond.dist)
    }
    distn
}

