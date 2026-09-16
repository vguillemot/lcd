#' Chain graph verification
#' 
#' Checks if a given graph is a chain graph.
#' 
#' 
#' @usage is.chaingraph(amat)
#' @param amat the adjacency matrix of the graph with dimnames.
#' @return \item{result }{a logical value, \code{TRUE} if the given graph is a
#' valid chain graph and \code{FALSE} otherwise.} \item{vert.order }{a
#' topological order of the graph vertices.} \item{chain.size }{a vector
#' indicating the size of each chain component in the graph, whose order
#' corresponds to \code{vert.order}.}
#' @author Zongming Ma and Xiangrui Meng
#' @references Cowell, R. G., Dawid, A. P., Lauritzen, S. L. and Spiegelhalter,
#' D. J. (1999) \emph{Probabilistic Networks and Expert Systems}.
#' Springer-Verlag, New York.
#' @keywords multivariate graphs models
#' @examples
#' 
#' data(lcd)
#' is.chaingraph(toy.graph)
#' 
`is.chaingraph` <-
function(amat)
{
    wmat <- matrix(as.integer((amat + t(amat)) > 1), nrow = nrow(amat))
    wg <- graph_from_adjacency_matrix(wmat, mode = "undirected")
    cc <- components(wg)
    neworder <- order(cc$membership)
    a <- matrix(0, nrow = length(cc$csize), ncol = length(cc$csize))
    b <- cumsum(cc$csize)
    wmat <- amat[neworder, neworder]
    for(i in 1: length(cc$csize)){
        for(j in 1: length(cc$csize)){
            if(j != i){
                a[i,j] <- as.integer(sum(wmat[(max(b[i-1],0)+1):b[i],
                                              (max(b[j-1],0)+1):b[j]]) > 0)
            }
        }
    }
    rownames(a) <- colnames(a) <- as.character(1:length(b))
    output <- isAcyclic(a)
    for(i in 1:length(b)){
        temp <- wmat[(max(b[i-1],0)+1):b[i], (max(b[i-1],0)+1):b[i]]
        if (!all(temp == t(temp))) {
            output <- FALSE
            break
        }
    }
    vertorder<-c()
    chainsize<-c()
    if(output == TRUE){
        chainorder <- topOrder(a)
        for(k in 1:length(b)){
            ## vertorder <- c(vertorder, which(cc$membership == chainorder[k]-1))
            vertorder <- c(vertorder, which(cc$membership == chainorder[k]))
            chainsize <- c(chainsize, cc$csize[chainorder[k]])
        }
    }
    return(list(result = output,
                vert.order = vertorder,
                chain.size = chainsize))
}

