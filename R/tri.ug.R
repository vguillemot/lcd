#' Triangulation of an undirected graph
#' 
#' Triangulates an undirected graph to a chordal graph.
#' 
#' The function implements the `One-step look ahead triangulation' algorithm
#' described in Cowell, et al (1999). The criterion \eqn{c(v)} is chosen to be
#' the number of edges needed to be added to the graph if vertex \eqn{v} is
#' chosen to be labelled.
#' 
#' @usage tri.ug(amat)
#' @param amat the adjacency matrix of undirected graph.
#' @return The adjacency matrix of the triangulated graph.
#' @author Zongming Ma and Xiangrui Meng
#' @references Cowell, R. G., Dawid, A. P., Lauritzen, S. L. and Spiegelhalter,
#' D. J. (1999) \emph{Probabilistic Networks and Expert Systems}.
#' Springer-Verlag, New York.
#' @keywords graphs
`tri.ug` <-
function(amat)
{
    k <- nrow(amat)
    unlab <- rep(TRUE, k)
    numorder <- c()
    counter <- k
    while(counter > 0){
        cc <- sample(which(unlab))
        minToFill <- k^2
        toLabel <- k+1
        Nb <- c()
        nNb <- 0
        for(j in 1:counter){
            cNb <- which(amat[cc[j],]==1 & unlab)
            cNb <- union(cNb, cc[j])
            ncNb <- length(cNb)
            toFill <- ncNb*(ncNb-1)/2 -
                sum(amat[cNb, cNb])/2
            if(toFill < minToFill){
                toLabel <- cc[j]
                Nb <- cNb
                minToFill <- toFill
                nNb <- ncNb
            }
        }
        if(nNb > 0 && minToFill > 0){
            newm <- matrix(1, nrow=nNb, ncol=nNb)
            diag(newm) <- 0
            amat[Nb, Nb] <- newm
        }
        numorder <- c(numorder, toLabel)
        unlab[toLabel] <- FALSE
        counter <- counter - 1
    }
    amat
}

