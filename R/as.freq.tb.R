#' Frequency table transformation
#' 
#' Transforms a discrete data matrix into a raw frequency table.
#' 
#' Each element in the \code{mat} is expected to an integer or a factor.
#' 
#' @usage as.freq.tb(mat)
#' @param mat a data matrix, with each row corresponding to an observation.
#' @return An object of class \code{freq.tb}
#' @author Zongming Ma and Xiangrui Meng
#' @seealso \code{\link{compress.freq.tb}}.
#' @keywords manip multivariate
`as.freq.tb` <- function(mat)
{
    n <- nrow(mat)
    p <- ncol(mat)
    vnames <- colnames(mat)
    mat <- matrix(as.integer(mat), n, p)
    levels <- apply(mat, 2, function(x) length(unique(x)))
    tb <- new("freq.tb",
              table = cbind(mat, as.integer(rep(1, n))),
              levels = levels
              )
    colnames(tb@table) <- c(vnames, "Freq")
    compress.freq.tb(tb)
}
