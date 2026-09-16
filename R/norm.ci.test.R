#' Conditional independence test for multivariate normal data
#' 
#' Performs a conditional independence test for multivariate normal variables
#' \code{u} and \code{v} with conditioning set \code{cond}.
#' 
#' The function performs a likelihood ratio test as described in Whittaker
#' (1990), Chapter 6.
#' 
#' @usage norm.ci.test(cov, n, u, v, cond = c())
#' @param cov the sample covariance matrix.
#' @param n number of samples used to compute \code{cov}.
#' @param u name of one variable under investigation.
#' @param v name of the other variable under investigation.
#' @param cond a vector of variable names indicating the conditioning set in
#' the test.
#' @return \item{deviance}{ the Chisq-statistic computed under the null
#' hypothesis. } \item{df}{ the degrees of freedom for the Chisq-statistic. }
#' \item{p.value}{ the \eqn{p}-value for the test. }
#' @section Note: Some of the \code{C} routines used were originally written by
#' Robert Castelo <robert.castelo@upf.edu>, Alberto Roverato
#' <alberto.roverato@unibo.it> in the \code{qp} package. The authors added the
#' part for calculate the likelihood ratio test-statistic and modified the
#' \code{R} wrapper function.
#' @author Zongming Ma and Xiangrui Meng
#' @references Whittaker, J. (1990). \emph{Graphical Models in Applied
#' Mathematical Multivariate Statistics.} John Wiley and Sons, Chichester,
#' England.
#' @keywords htest multivariate
`norm.ci.test` <- function(cov, n, u, v, cond = c())
{
    vset <- rownames(cov)
    if(is.character(u)) u <- which(vset == u)
    if(is.character(v)) v <- which(vset == v)
    if(is.character(cond)) cond <- match(cond, vset)
    if(!is.numeric(u) || !is.numeric(v) || (!is.numeric(cond) && length(cond) > 0))
        stop("Invalid vertex!")
    res <- .Call("g_ci_test", cov,
                 as.integer(n),
                 as.integer(u), as.integer(v), as.integer(cond),
                 PACKAGE = "lcd")
    if (is.na(res[1])) {
        res[3] <- 0
        res[1] <- qchisq(1 - 1e-16, 1)
    }
    return(list(deviance = res[1], df = res[2], p.value = res[3]))
}
