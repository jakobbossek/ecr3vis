#' @title
#' Dominance relation checks
#'
#' @description
#' Check if a vector dominates another (\code{dominates} or \code{does_dominate})
#' or is dominated by another (\code{is_dominated}). There are corresponding infix
#' operators \code{dominates} and \code{is_dominated}.
#'
#' @keywords optimize
#'
#' @param x [\code{numeric}]\cr
#'   First vector.
#' @param y [\code{numeric}]\cr
#'   Second vector.
#' @return [\code{logical(1)}]
#' @rdname dominates
#' @family pareto-dominance
#' @family mootools
#' @export
dominates = function(x, y) {
  stopifnot(length(x) == length(y))
  return(all(x <= y) && any(x < y))
}

#' @rdname dominates
#' @export
does_dominate = function(x, y) {
  dominates(x, y)
}

#' @rdname dominates
#' @export
is_dominated = function(x, y) {
  return(dominates(y, x))
}

#' @rdname dominates
#' @export
`%dominates%` = function(x, y) {
  return(dominates(x, y))
}

#' @rdname dominates
#' @export
`%is_dominated%` = function(x, y) {
  return(dominates(y, x))
}

#' @title
#' Check for Pareto-dominance.
#'
#' @description
#' These functions take a numeric matrix as input where each column corresponds to
#' a point and return a logical vector. The \eqn{i}th position of the latter is
#' \code{TRUE} if the \eqn{i}th point is dominated by at least one other point for
#' \code{dominated} and \code{FALSE} for \code{nondominated}.
#'
#' @keywords optimize
#'
#' @param x [\code{matrix}]\cr
#'   Numeric \eqn{(m \times n)} matrix where \eqn{m} is the number of objectives
#'   and \eqn{n} is thenumber of points.
#' @return [\code{logical}]
#' @rdname dominated
#' @family pareto-dominance
#' @family mootools
#' @export
dominated = function(x) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  return(.Call("dominated_c", x))
}

#' @rdname dominated
#' @export
nondominated = function(x) {
  return(!dominated(x))
}

#' @title Determine which points of a set are (non)dominated.
#'
#' @description
#' Given a matrix with one point per column \code{which_dominated} returns the
#' column numbers of the dominated points and \code{which_nondominated} the column
#' numbers of the nondominated points. Function \code{is_maximally_dominated} returns
#' a logical vector with \code{TRUE} for each point which is located on the last
#' non-domination level.
#'
#' @keywords optimize
#'
#' @param x [\code{matrix}]\cr
#'   Numeric \eqn{(m \times n)} matrix where \eqn{n} is the number of points and \eqn{m}
#'   is the number of objectives.
#' @return [\code{integer}]
#' @examples
#'   data(mtcars)
#'   # assume we want to maximize horsepower and minimize gas consumption
#'   cars = mtcars[, c("mpg", "hp")]
#'   cars$hp = -cars$hp
#'   idxs = which_nondominated(t(as.matrix(cars)))
#'   \dontrun{
#'   print(mtcars[idxs, ])
#'   plot(cars)
#'   points(cars[idxs, ], col = "tomato", pch = 10)
#'   }
#' @rdname which_dominated
#' @family pareto-dominance
#' @family mootools
#' @export
which_dominated = function(x) {
  which(dominated(x))
}

#' @rdname which_dominated
#' @export
which_nondominated = function(x) {
  which(!dominated(x))
}

#' @rdname which_dominated
#' @export
is_maximally_dominated = function(x) {
  checkmate::assert_matrix(x, min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  ranks = nds(x)$ranks
  ranks == max(ranks)
}

#' @title
#' Check if one set is better than another.
#'
#' @description
#' The function checks, whether every point from the second set of points
#' is dominated by at least one point from the first set.
#'
#' @param x [\code{matrix}]\cr
#'   First set of points.
#' @param y [\code{matrix}]\cr
#'   Second set of points.
#' @return [\code{logical(1)}]
#' @family pareto-dominance
#' @family mootools
#' @export
set_dominates = function(x, y) {
  n1 = ncol(x)
  n2 = ncol(y)
  z = cbind(x, y)

  dom = dominated(z)
  all(dom[(n1 + 1L):(n1 + n2)])
}
