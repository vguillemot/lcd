#' Graph skeleton
#' 
#' Returns the skeleton of a graph. The graph can be undirected, directed or
#' mixed.
#' 
#' 
#' @usage skeleton(amat)
#' @param amat the adjacency matrix of the graph.
#' @return The adjacency matrix of the graph skeleton.
#' @author Zongming Ma and Xiangrui Meng
#' @keywords graphs
#' @examples
#' 
#' skeleton(toy.graph)
#' 
`skeleton` <- function(amat)
{
    0 + (amat + t(amat) > 0)
}
