#' Maximum cardinality search
#' 
#' Performs a maximum cardinality search on an undirected graph to determine
#' whether it is triangulated.
#' 
#' 
#' @usage maxcard.search(amat)
#' @param amat the adjacency matrix of the undirected graph.
#' @return \item{is.triangulated }{a logical value indicating whether the input
#' graph is triangulated or not.} \item{perfect.numbering }{a perfect numbering
#' of the vertices.} \item{card }{ number of unlabeled neighbors when labeling
#' each variable, with order compatible to the perfect numbering.}
#' \item{pi.record }{a record of unlabeled neighbors during the execution of
#' the algorithm.}
#' @note Only the \code{is.triangulated} and \code{perfect.numbering} are
#' supposed to be of interest to the users. The other two output components are
#' mainly for track purpose.
#' @author Zongming Ma and Xiangrui Meng
#' @references Tarjan, R. E. and Yannakakis, M. (1984). Simple linear-time
#' algorithms to test chordality of graphs, test acyclicity of hypergraphs, and
#' selectively reduce acyclic hypergraphs. \emph{SIAM Journal on Computing},
#' \bold{13}, 566-79.
#' @keywords graphs
`maxcard.search` <-
function(amat)
{
    output <- TRUE
    k <- nrow(amat)
    L <- c()
    V <- 1:k
    C <- rep(0,k)
    card <- pi_record <- c()
    for(j in 1:k){
        U <- setdiff(V, L)
        pos <- which(C[U] == max(C[U]))
        if(length(pos) > 1){
            v <- U[sample(pos,1)]
        } else {
            v <- U[pos]
        }
        nb <- which(amat[v,]==1)
        pi_v <- intersect(nb, L)
        m <- length(pi_v)
        card <- c(card, m)
        pi_record <- c(pi_record, pi_v)
        ne <- sum(amat[pi_v, pi_v])/2
        if(ne < m*(m-1)/2){
            output <- FALSE
            break
        }
        nbU <- intersect(nb, U)
        C[nbU] <- C[nbU] + 1
        L <- c(L, v)
    }
    return(list(is.triangulated = output,
                perfect.numbering = L,
                card = card,
                pi.record = pi_record))
}
