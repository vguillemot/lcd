#' c-separation on the chain graph
#' 
#' Checks whether two vertices \code{u} and \code{v} are \eqn{c}-separated by a
#' set \code{sep} on the given chain graph.
#' 
#' The actual function uses the moralization criterion instead of the
#' separation criterion. They are equivalent and the former is easier to
#' implement.
#' 
#' @usage is.separated(u, v, sep, amat)
#' @param u one vertex under investigation.
#' @param v the other vertex under investigation.
#' @param sep the candidate separator.
#' @param amat the adjacency matrix of a chain graph.
#' @return A boolean value indicating whether separation holds or not.
#' @author Zongming Ma and Xiangrui Meng
#' @references Lauritzen, S. L. (1996). \emph{Graphical Models.} Clarendon
#' Press, Oxford.
#' 
#' Studeny, M. and Bouckaert, R. R. (1998). On chain graph models for
#' description of conditional independence structures. \emph{Annals of
#' Statistics} \bold{26} 1434-1495.
#' @keywords graphs
#' @examples
#' 
#' is.separated("a", "d", c("b","c"), toy.graph)
#' 
is.separated <- function(u, v, sep, amat)
{
    vset <- rownames(amat)
    curr <- c(u, v, sep)
    s <- length(curr)
    q <- curr
    if(s > 0) {
        for(i in 1:s)
            q <- union(q,
                       vset[sapply(vset, function(x)
                                   .is.linked(amat, curr[i], x))])
    }
    curr <- union(curr, q)
    while(TRUE){
        cc <- pa(curr, amat)
        cc <- setdiff(cc, curr)
        scc <- length(cc)
        if (scc > 0) {
            cccliq <- cc
            for(j in 1:scc)
                cccliq <- union(cccliq,
                                vset[sapply(vset, function(x)
                                            .is.linked(amat, cc[j], x))])
            curr <- union(curr, cccliq)
        } else {
            break
        }
    }
    anamat <- amat[curr, curr]
    anamat <- moralize(anamat)
    idx <- match(sep, rownames(anamat))
    !.is.linked(anamat[-idx, -idx], u, v)
}


