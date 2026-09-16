#' Draw graph
#' 
#' Draws a graph from its adjacency matrix.
#' 
#' The function converts the adjacency matrix to an \code{igraph} object and
#' uses the \code{plot.igraph}/\code{tkplot} function in \code{igraph} package
#' to draw the graph. Special care is paid to make the directed/undirected
#' edges displayed right. Finally, the function returns the vertex list of the
#' graph for comparison to the vertex numbering used in the plot.
#' 
#' @usage draw(amat, plain = TRUE)
#' @param amat the adjacency matrix of the graph.
#' @param plain logical value. If \code{TRUE}, then a plain plot is drawn for
#' the graph; if \code{FALSE}, a tlkck plot is drawn.
#' @return The vertex list. The main output is the side effect of the function.
#' @author Zongming Ma and Xiangrui Meng
#' @keywords graphs iplot
`draw` <-
    function(amat, plain = TRUE){
    e.total <- length(which(skeleton(amat) == 1))/2
    if(e.total == 0)
        return("The graph has no edge!")
    ## el <- matrix("", nrow = e.total, ncol = 2)
    el <- matrix(NA, nrow = e.total, ncol = 2)
    vset <- sort(rownames(amat))
    amat <- amat[vset,vset]
    p <- nrow(amat)
    arrow <- c()
    k <- 1
    for(i in 1:(p-1))
        for(j in (i+1):p){
            if(amat[i,j] == 1){
                ## el[k,1] <- i-1
                el[k,1] <- i
                ## el[k,2] <- j-1
                el[k,2] <- j
                k <- k + 1
                if(amat[j,i] == 0){
                    arrow <- c(arrow, ">")
                } else {
                    arrow <- c(arrow, "-")
                }
            } else {
                if(amat[j,i] == 1){
                  ## el[k,1] <- j-1
                  el[k,1] <- j
                  ## el[k,2] <- i-1
                  el[k,2] <- i
                    k <- k + 1
                    arrow <- c(arrow, ">")
                }
            }
        }
    g <- make_empty_graph()
    g <- add_vertices(g, p)
    V(g)$name <- vset
    g <- add_edges(g, t(el))
    if(plain){
        plot.igraph(g, #layout = layout_as_tree,
                    edge.arrow.mode = arrow,
                    vertex.size = min(15, 500/p),
                    edge.arrow.size = min(1, 30/p))
    } else{
        tkplot(g, layout = layout_as_tree,
               edge.arrow.mode = arrow,
               vertex.size = min(15, 500/p),
               edge.arrow.size = min(1, 30/p))
    }
    V(g)
}
