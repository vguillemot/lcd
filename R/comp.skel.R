#' Skeleton comparison
#' 
#' Compares a graph skeleton to the (supposed) true skeleton. The two skeletons
#' should have the same set of vertices in order for the function to return a
#' meaningful result.
#' 
#' 
#' @usage comp.skel(trueskel, skel)
#' @param trueskel the adjacency matrix of the true skeleton.
#' @param skel the adjacency matrix of the skeleton to be compared with the
#' true one.
#' @return \item{e.total }{total number of edges on the true skeleton.}
#' \item{e.missing }{number of true edges missing in the skeleton to be
#' compared.} \item{e.extra }{number of spurious edges present in the skeleton
#' to be compared.}
#' @seealso \code{\link{comp.pat}}.
#' @keywords graphs
`comp.skel` <- function(trueskel, skel)
{
    vset <- rownames(trueskel)
    a <- trueskel - skel[vset, vset]
    e.missing <- length(which(a == 1))/2
    e.extra <- length(which(a == -1))/2
    return(list(e.total = length(which(trueskel == 1))/2,
                e.missing = e.missing,
                e.extra = e.extra))
}
