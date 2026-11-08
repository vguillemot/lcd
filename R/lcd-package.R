

#' Graph structure of ALARM network
#' 
#' The adjacency matrix of the ALARM network graph structure.
#' 
#' 
#' @name alarm.net
#' @docType data
#' @usage data(alarm.net)
#' @source Beinlich, I., Suermondt, H., Chavez, R. and Cooper, G. (1989). The
#' ALARM monitoring system: A case study with two probabilistic inference
#' techniques for belief networks, in: \emph{Proceedings of the 2nd European
#' Conference on Artificial Intelligence in Medicine}, pp.247-256,
#' Springer-Verlag, Berlin.
#' @keywords datasets
NULL





#' All.equal method for sep.pair class
#' 
#' Checks if two \code{sep.pair} objects are all.equal. We call two
#' \code{sep.pair} objects all.equal if they have the same (\code{u},\code{v})
#' set, regardless of order.
#' 
#' 
#' @name all.equal-methods
#' @aliases all.equal-methods all.equal,sep.pair,sep.pair-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{target = "sep.pair", current = "sep.pair"}{ Returns \code{TRUE} if
#' \code{target} and \code{current} have the same (\code{u},\code{v}) set. } }
#' @return A single logical value: \code{TRUE} if \code{target} and
#' \code{current} have the same (\code{u},\code{v}) set (regardless of
#' order), \code{FALSE} otherwise.
#' @keywords methods
NULL





#' Class "freq.tb"
#' 
#' Objects representing frequency counts of all configurations of discrete
#' variables existing in a data set.
#' 
#' 
#' @name freq.tb-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("freq.tb", ...)}.
#' @slot table An \eqn{m} by \eqn{p+1} matrix. The first \eqn{p} integers in
#' each row give a configuration, and the final element records its frequency.
#' @slot levels A \eqn{p}-vector giving the number of levels for each variable.
#' @section Warning : The user is responsible for giving the correct
#' \code{levels}!
#' @author Zongming Ma and Xiangrui Meng
#' @keywords classes
#' @examples
#' 
#' showClass("freq.tb")
#' 
NULL





#' Structural learning of chain graphs via the decomposition approach
#' 
#' This package implements constraint-based algorithms for learning chain
#' graphs, including Bayesian networks and Markov networks, from continuous or
#' discrete data. Following Ma et al. (2008)
#' <https://jmlr.org/papers/v9/ma08a.html>, it decomposes the learning problem
#' into smaller subproblems and provides algorithms for skeleton recovery and
#' edge orientation, without requiring complete separators. The package also
#' provides tools for graph manipulation, conditional independence testing,
#' and sampling from chain-graph models.
#' 
#' 
#' @name lcd-package
#' @aliases lcd-package lcd
#' @keywords internal
#' @useDynLib lcd
#' @importFrom MASS mvrnorm loglm
#' @importFrom ggm transClos isAcyclic topOrder pa
#' @importFrom igraph V V<- add_edges add_vertices cliques components
#'   graph_from_adjacency_matrix layout_as_tree make_empty_graph plot.igraph tkplot
#' @importFrom methods new show validObject
#' @importFrom stats as.formula cor cov2cor pchisq qchisq qt rbeta rnorm runif
#' @export as.freq.tb
#' @export comp.pat
#' @export comp.skel
#' @export compress.freq.tb
#' @export draw
#' @export get.multinom.dist
#' @export get.normal.dist
#' @export is.chaingraph
#' @export is.separated
#' @export learn.complex.multinom
#' @export learn.complex.norm
#' @export learn.mec.multinom
#' @export learn.mec.norm
#' @export learn.skeleton.multinom
#' @export learn.skeleton.norm
#' @export learn.v
#' @export maxcard.search
#' @export moralize
#' @export multinom.ci.test
#' @export naive.getug.multinom
#' @export naive.getug.norm
#' @export norm.ci.test
#' @export pattern
#' @export rmultinom.cg
#' @export rnorm.cg
#' @export skeleton
#' @export tri.ug
#' @export ug.to.jtree
#' @exportClass sep.pair
#' @exportClass sep.tree
#' @exportClass freq.tb
#' @section List of functions: The package has the following categories of
#' functions.  \itemize{ \item Chain graph/Bayesian network learning:
#' \code{\link{learn.mec.norm}}, \code{\link{learn.mec.multinom}},
#' \code{\link{learn.skeleton.norm}}, \code{\link{learn.skeleton.multinom}},
#' \code{\link{learn.complex.norm}}, \code{\link{learn.complex.multinom}},
#' \code{\link{learn.v}}.
#' 
#' \item Graphical model manipulation and graph plotting: \code{\link{draw}},
#' \code{\link{is.chaingraph}}, \code{\link{is.separated}},
#' \code{\link{moralize}}, \code{\link{pattern}}, \code{\link{skeleton}},
#' \code{\link{tri.ug}}, \code{\link{maxcard.search}},
#' \code{\link{ug.to.jtree}}.
#' 
#' \item Random distribution and random sample generation from chain graph:
#' \code{\link{get.normal.dist}}, \code{\link{get.multinom.dist}},
#' \code{\link{rnorm.cg}}, \code{\link{rmultinom.cg}}.
#' 
#' \item Conditional independence tests: \code{\link{norm.ci.test}},
#' \code{\link{multinom.ci.test}}.
#' 
#' \item Construction of undirected independence graphs from data:
#' \code{\link{naive.getug.norm}}, \code{\link{naive.getug.multinom}}.
#' 
#' \item Frequency table manipulation: \code{\link{as.freq.tb}},
#' \code{\link{compress.freq.tb}}.
#' 
#' \item Graph Comparison: \code{\link{comp.pat}}, \code{\link{comp.skel}}.  }
#' Moreover, the package include the following two graph structure: \itemize{
#' \item The ALARM network: \code{\link{alarm.net}}; \item A toy example:
#' \code{\link{toy.graph}}.  } % The package is intended to be a contribution
#' to the gR-project % described in Lauritzen (2002).
#' @author Zongming Ma\cr Department of Statistics\cr The Wharton School,
#' University of Pennsylvania, USA\cr \email{zongming.ma@@gmail.com}
#' 
#' Xiangrui Meng\cr Institute of Computational and Mathematical Engineering\cr
#' Stanford University, USA
#' @references Lauritzen, S. L. (2002). gRaphical Models in R. \emph{R News},
#' 3(2), 39. Ma, Z., Xie, X. and Geng, Z. (2008). Structural learning of
#' chain graphs via decomposition. \emph{J. Mach. Learn. Res.,} \bold{9},
#' 2847-2880.
#' 
#' Xie, X., Geng, Z. and Zhao, Q. (2006). Decomposition of structural learning
#' about directed acyclic graphs. \emph{Artif.  Intell.,} \bold{170}, 422-439.
#' @keywords package multivariate models graphs
#' @examples
#' 
#' set.seed(100)
#' p.value <- .01
#' n <- 3000
#' is.chaingraph(toy.graph)
#' tgdata <- rnorm.cg(n, toy.graph, get.normal.dist(toy.graph))
#' tgug <- naive.getug.norm(tgdata, p.value)
#' tg.jtree <- ug.to.jtree(tgug)
#' tg.pat <- learn.mec.norm(tg.jtree, cov(tgdata), n, p.value, "CG")
#' comp.skel(skeleton(toy.graph), skeleton(tg.pat))
#' comp.pat(pattern(toy.graph), tg.pat)
#' 
"_PACKAGE"





#' Graph learning functions
#' 
#' Learns chain graphs structures via decomposition algorithms with normal or
#' multinomial data.
#' 
#' \code{learn.mec.norm} is the wrapper function for
#' \code{learn.skeleton.norm}, \code{learn.complex.norm} and \code{learn.v} for
#' normal data, which is intended to be mostly called by the user.
#' \code{learn.mec.multinom} is the counterpart for discrete data. The user
#' specifies whether she/he is learning an undirected graph (Markov network), a
#' DAG (Bayesian network) or in general a chain graph by supplying the specific
#' string to the \code{method} argument. The function itself will utilize the
#' correct helper functions to perform the structural learning and return the
#' pattern (representing the Markov equivalent class) of the graph.
#' 
#' \code{learn.skeleton.norm} and \code{learn.skeleton.multinom} try to learn
#' the skeleton of the graph. \code{learn.v}, \code{learn.complex.norm} (or
#' \code{learn.complex.multinom}) are for v-structure/complex discovery in
#' DAG/chain graph respectively.
#' 
#' Some parts of the code of these learning functions are adapted from the code
#' for \code{pcAlgo} function in the \code{R} package \code{pcalg} written by
#' Markus Kalisch and Martin Maechler.
#' 
#' @name learn.graph
#' @aliases learn.complex.norm learn.complex.multinom learn.mec.norm learn.mec.multinom learn.skeleton.norm learn.skeleton.multinom learn.v
#' @usage
#' 
#' learn.complex.norm(skel, cov, n, p.value)
#' 
#' learn.mec.norm(tree, cov, n, p.value, method = "CG")
#' 
#' learn.skeleton.norm(tree, cov, n, p.value, drop = TRUE)
#' 
#' learn.complex.multinom(skel, freq.tb, p.value)
#' 
#' learn.mec.multinom(tree, freq.tb, p.value, method = "CG")
#' 
#' learn.skeleton.multinom(tree, freq.tb, p.value, drop = TRUE)
#' 
#' learn.v(skel, tree)
#' @param skel the object returned by \code{learn.skeleton.norm} function.
#' @param tree an object of class \code{sep.tree}, e.g., the separation tree
#' obtained via \code{ug.to.jtree}.
#' @param cov the covariance matrix of the data
#' @param n number of random samples used to obtain the covariance matrix.
#' @param freq.tb the frequency table of the data, should be an object from
#' class \code{freq.tb}.
#' @param p.value thresholding \eqn{p}-value for conditional independence test
#' during the learning procedure.
#' @param method a character string, must be one of "CG", "DAG" and "UG",
#' telling the alogrithm whether the underlying graph is a DAG, undirected
#' graph or more generally a chain graph.
#' @param drop logical value, with default=\code{TRUE}, whether to drop
#' possibly extra edges after recovering and combining local skeletons.
#' @return \code{learn.mec.norm}, \code{learn.mec.multinom}, \code{learn.v},
#' \code{learn.complex.norm}, \code{learn.complex.multinom} return the pattern
#' of the graph, represented in its adjacency matrix.
#' 
#' \code{learn.skeleton.norm} and \code{learn.skeleton.multinom} return a list
#' of the following two items: \item{amat }{the adjacency matrix of the learned
#' skeleton.} \item{sep.pairs }{a list of \code{sep.pair} objects, recording
#' the separation pairs we obtained during the skeleton learning procedure.}
#' @author Zongming Ma and Xiangrui Meng
#' @references Ma, Z., Xie, X. and Geng, Z. (2008). Structural learning of
#' chain graphs via decomposition. \emph{J. Mach. Learn. Res.,} \bold{9},
#' 2847-2880.
#' 
#' Xie, X., Geng, Z. and Zhao, Q. (2006). Decomposition of structural learning
#' about directed acyclic graphs. \emph{Artif.  Intell.,} \bold{170}, 422-439.
#' @keywords graphs multivariate models
#' @examples
#' 
#' set.seed(100)
#' p.value <- .01
#' n <- 3000
#' is.chaingraph(toy.graph)
#' tgdata <- rnorm.cg(n, toy.graph, get.normal.dist(toy.graph))
#' tgug <- naive.getug.norm(tgdata, p.value)
#' tg.jtree <- ug.to.jtree(tgug)
#' tg.pat <- learn.mec.norm(tg.jtree, cov(tgdata), n, p.value, "CG")
#' comp.skel(skeleton(toy.graph), skeleton(tg.pat))
#' comp.pat(pattern(toy.graph), tg.pat)
#' 
NULL





#' Class "sep.pair"
#' 
#' Objects representing separation pairs in a graphical model, with slots
#' \code{u} and \code{v} for the pair of vertices and slot \code{s} for their
#' separator, which indicates the relation \code{u} and \code{v} are separated
#' by \code{s} (on some graph \eqn{G}).
#' 
#' 
#' @name sep.pair-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("sep.pair", ...)}.
#' @slot u A character vector containing the first vertex of the pair.
#' @slot v A character vector containing the second vertex of the pair.
#' @slot s A character vector containing the separator of \code{u} and
#' \code{v}.
#' @section Methods: \describe{ \item{all.equal}{\code{signature(target =
#' "sep.pair", current = "sep.pair")}: see \code{\link{all.equal}}. }
#' \item{show}{\code{signature(object = "sep.pair")}.} }
#' @keywords classes
#' @examples
#' 
#' showClass("sep.pair")
#' 
NULL





#' Class "sep.tree"
#' 
#' Objects representing separation tree as described in Xie, Geng and Zhao
#' (2006) and Ma, Xie and Geng (2008), which includes junction tree of cliques
#' as a special case.
#' 
#' 
#' @name sep.tree-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("sep.tree", ...)}. We recommend user to use the functions provided
#' in this package, such as \code{\link{ug.to.jtree}} to create the separation
#' tree object.
#' @slot tree.struct The adjacency matrix of the separation tree.
#' @slot cliques The list of cliques in the separation tree.
#' @slot separators A list of separators. Each separator has a
#' \code{separator} component containing graph vertices and an \code{edge}
#' component identifying the tree edge to which it is attached.
#' @author Zongming Ma and Xiangrui Meng
#' @references Ma, Z., Xie, X. and Geng, Z. (2008). Structural learning of
#' chain graphs via decomposition. \emph{J. Mach. Learn. Res.,} \bold{9},
#' 2847-2880.
#' 
#' Xie, X., Geng, Z. and Zhao, Q. (2006). Decomposition of structural learning
#' about directed acyclic graphs. \emph{Artif.  Intell.,} \bold{170}, 422-439.
#' @keywords classes
#' @examples
#' 
#' showClass("sep.tree")
#' 
NULL





#' Show method for sep.pair class
#' 
#' Method for showing a \code{sep.pair} object.
#' 
#' 
#' @name show-methods
#' @aliases show-methods show,sep.pair-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{object = "sep.pair"}{ Successively prints out the pairs (\code{u},
#' \code{v}) and their separator \code{s}. } }
#' @return No return value, called for side effects (prints the
#' \code{sep.pair} object to the console).
#' @keywords methods
NULL





#' Graph structure of a toy example
#' 
#' The adjacency matrix of a toy chain graph example.
#' 
#' 
#' @name toy.graph
#' @docType data
#' @usage data(toy.graph)
#' @keywords datasets
NULL


