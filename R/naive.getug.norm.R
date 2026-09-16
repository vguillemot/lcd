#' A naive function to get an undirected graph for normal data
#' 
#' Learns an undirected independence graph from a given data set. The data are
#' assumed to be normally distributed.
#' 
#' For each pair of random variables, the function performs a conditional
#' independence using the partial correlation coefficient. If the p-value of
#' the test is smaller than the given threshold, then there will be an edge on
#' the output graph. The function essentially uses the global Markov property
#' of the undirected graph.
#' 
#' @usage naive.getug.norm(data, p.value)
#' @param data a data matrix with rows corresponding to observations and
#' columns corresponding to random variables.
#' @param p.value the thresholding \eqn{p}-value for each conditional
#' independence test.
#' @return The adjacency matrix of the computed undirected graph, with dimnames
#' the corresponding random variables.
#' @author Zongming Ma and Xiangrui Meng
#' @references Lauritzen, S. L. (1996). \emph{Graphical Models.} Clarendon
#' Press, Oxford.
#' @keywords multivariate graphs models
`naive.getug.norm` <-
function(data, p.value)
{
    conmat <- solve(cor(data))
    p <- ncol(data)
    n <- nrow(data)
    parcor <- cov2cor(conmat)
    t.stat <- parcor*sqrt((n-p-2)/(1-parcor^2))
    thres <- -qt(p.value/2, df=n-p-2)
    amat <- matrix(as.integer(abs(t.stat)>thres), nrow=p, ncol=p)
    diag(amat) <- rep(0,p)
    rownames(amat) <- colnames(amat) <- rownames(conmat)
    amat
}

