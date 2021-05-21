#' @title Fast non-dominated sorting
#'
#' @description
#' Fast non-dominated sorting algorithm proposed by Deb et al. [1].
#' Non-dominated sorting expects a set of points and returns a
#' set of non-dominated fronts. In short words this is done as follows: the
#' non-dominated points of the entire set are determined and assigned rank 1.
#' Afterwards all points with the current rank are removed, the rank is increased
#' by one and the procedure starts again. This is done until the set is empty, i.~e.,
#' each point is assigned a rank.
#'
#' @note
#' This procedure is the key survival selection of the famous NSGA-II multi-objective
#' evolutionary algorithm.
#'
#' @references
#' [1] Deb, K., Pratap, A., and Agarwal, S. A Fast and Elitist Multiobjective Genetic
#' Algorithm: NSGA-II. IEEE Transactions on Evolutionary Computation, 6 (8) (2002),
#' 182-197.
#'
#' @keywords optimize
#'
#' @param x [\code{matrix}]\cr
#'   Numeric matrix of points. Each column contains one point.
#' @return [\code{list}]
#'   List with the following components
#'   \describe{
#'     \item{ranks}{Integer vector of ranks of length \code{ncol(x)}. The higher
#'     the rank, the higher the domination front the corresponding point is
#'     located on.}
#'     \item{dom.counter}{Integer vector of length \code{ncol(x)}. The i-th element
#'     is the domination number of the i-th point.}
#'   }
#' @family mootools
#' @export
nds = function(x) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  return(.Call("nds_c", x))
}
