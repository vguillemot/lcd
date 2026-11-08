#' Conditional independence test for multinomial data
#' 
#' Performs a conditional independence test for variables \code{u} and \code{v}
#' with conditioning set \code{cond}.
#' 
#' The test is based on the deviance test in log-linear model for count data.
#' The null hypothesis is that all interaction terms involving both \code{u}
#' and \code{v} vanish.
#' 
#' @usage multinom.ci.test(tb, u, v, cond = c())
#' @param tb a frequency table summarizing the data, an object of class
#' \code{freq.tb}.
#' @param u name of one variable under investigation.
#' @param v name of the other variable under investigation.
#' @param cond a vector of variable names indicating the conditioning set in
#' the test.
#' @return \item{deviance}{ the deviance of the saturated model from the model
#' under null hypothesis.} \item{df}{ the degrees of freedom for the deviance.}
#' \item{p.value}{ the approximate \eqn{p}-value for the test based on
#' Chi-square approximation.}
#' @author Zongming Ma and Xiangrui Meng
#' @references Agresti, A. (2002). \emph{Categorical Data Analysis.} 2nd Ed.
#' John Wiley and Sons. Hoboken, NJ.
#' @keywords htest multivariate
 `multinom.ci.test` <- function(tb, u, v, cond = c())
{
    if(!inherits(tb, "freq.tb"))
        stop("tb should be an object of class freq.tb!")
    if(!all(is.na(match(c(u,v), cond))))
        stop("The conditional set should not contain u or v!")
    if (nrow(tb@table) == 1) wtb <- tb
    else wtb <- compress.freq.tb(tb, c(u, v, cond))
    res <- .Call("cond_ind_test",
                 as.matrix(wtb@table),
                 as.vector(wtb@levels),
                 as.integer(0),
                 as.integer(1),
                 PACKAGE = "lcd")
    return(list(deviance = res[1], df = res[2], p.value = res[3]))
}
