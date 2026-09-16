 




#' Frequency table compression
#' 
#' Compresses a frequency table to a subset of the variables.
#' 
#' The actual computation routine is implemented in \code{C++}.
#' 
#' @usage compress.freq.tb(tb, subset = colnames(tb@table)[-ncol(tb@table)])
#' @param tb the frequency table to be compressed, an object of class
#' \code{freq.tb}.
#' @param subset a vector of strings indicating the subset of variables to be
#' compressed on. The default is the whole variable set.
#' @return The compressed table, an object of class \code{freq.tb}.
#' @author Zongming Ma and Xiangrui Meng
#' @keywords manip multivariate
`compress.freq.tb` <- function(tb, subset = colnames(tb@table)[-ncol(tb@table)])
{
    vnames <- colnames(tb@table)
    last <- length(vnames)
    idx <- match(subset, vnames[-last])
    if(any(is.na(idx)))
        stop("Invalid subset!")
    newlev <- tb@levels[idx]
    idx <- c(idx, last)
    newtb <- .Call("compress_freq_table",
                   tb@table[,idx],
                   PACKAGE = "lcd")
    colnames(newtb) <- vnames[idx]
    new("freq.tb", table = newtb, levels = newlev)
}
