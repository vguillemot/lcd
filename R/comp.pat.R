#' Pattern comparison
#' 
#' Compares a (learned) chain graph pattern to the (supposed) true pattern. The
#' two patterns should have the same vertex set in order for the function to
#' return a meaningful result.
#' 
#' 
#' @usage comp.pat(truepat, pat)
#' @param truepat the adjacency matrix of the true pattern.
#' @param pat the adjacency matrix of the pattern to be compared with the true
#' one.
#' @return \item{a.total }{total number of complex arrows on the true pattern.}
#' \item{a.missing}{number of true complex arrows missing in the pattern to be
#' compared.} \item{a.extra }{number of spurious complex arrows present in the
#' pattern to be compared.} \item{shd }{structural Hamming distance from
#' \code{pat} to \code{truepat}.}
#' @note Structural Hamming distance is defined as the total number of
#' operations needed to convert one graph to the other. Each operation must be
#' one of the following: (1) add or delete an undirected edge, or (2) add,
#' remove or reverse an orientation of an edge.
#' @author Zongming Ma and Xiangrui Meng
#' @seealso \code{\link{comp.skel}}.
#' @references Tsamardinos, I., Brown, L., and Aliferis, C. (2006). The max-min
#' hill-climbing Bayesian network structure learning algorithm. \emph{Mach.
#' Learn.}, \bold{65}(1):31-78.
#' @keywords graphs
`comp.pat` <- function(truepat, pat)
{
    vset <- rownames(truepat)
    truearr <- which(truepat - t(truepat) == 1)
    pat <- pat[vset, vset]
    arr <- which(pat - t(pat) == 1)
    a.missing <- length(truearr)-length(which(match(truearr, arr)>0))
    a.extra <- length(arr)-length(which(match(arr, truearr)>0))
    
    ## computing structural Hamming distance

    idx <- which(skeleton(truepat) - skeleton(pat) != 0)
    pat[idx] <- skeleton(truepat)[idx]
    shd <- length(idx) / 2
    c <- abs(truepat-pat)
    shd <- shd + length(which(c+t(c) != 0)) / 2
    return(list(a.total = length(truearr),
                a.missing = a.missing,
                a.extra = a.extra,
                shd = shd))
}
