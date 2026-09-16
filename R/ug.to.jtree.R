#' Junction tree construction for undirected graph
#' 
#' Constructs a junction tree for an undirected graph.
#' 
#' This function and its helpers implement the junction tree construction
#' algorithm described in detail in Section 4.3 and 4.4 of Cowell, et al
#' (1999).
#' 
#' @usage ug.to.jtree(ugamat)
#' @param ugamat the adjacency matrix of the undirected graph.
#' @return An object of class \code{sep.tree}.
#' @author Zongming Ma and Xiangrui Meng
#' @seealso \code{\link{sep.tree-class}}.
#' @references Cowell, R. G., Dawid, A. P., Lauritzen, S. L. and Spiegelhalter,
#' D. J. (1999) \emph{Probabilistic Networks and Expert Systems}.
#' Springer-Verlag, New York.
#' @keywords graphs
`ug.to.jtree` <-
function(ugamat)
{
    vset <- rownames(ugamat)
    a <- maxcard.search(tri.ug(ugamat))
    tree <- .junc.tree(.find.clique(a), vset)
    new("sep.tree",
        tree.struct = tree$tree.struct,
        cliques = tree$cliques,
        separators = tree$separators)
}
