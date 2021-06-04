#' @title
#' Dominance relation checks
#'
#' @description
#' Check if a vector dominates another (\code{dominates} or \code{does_dominate})
#' or is dominated by another (\code{is_dominated}). There are corresponding infix
#' operators \code{dominates} and \code{is_dominated}.
#'
#' @details
#' Given two vectors \eqn{x,y \in R^m} we say that \eqn{x} dominates \eqn{y},
#' denoted as \eqn{x \preceq y} if and only if
#' \deqn{
#'   x_i \leq y_i \, \forall i \in \{1, \ldots, m\}
#' }
#' and
#' \deqn{
#'   \exists j \in \{1, \ldots, m\}: x_i < y_i.
#' }
#' Informally, \eqn{x} dominates \eqn{y} if \eqn{x} is no worse than \eqn{y} in
#' all components and \eqn{x} is strictly better than \eqn{y} in at least one
#' component. Note that this definition focuses on minimization of all
#' objectives. This is no restriction, since the maximization of some function
#' is equivalent to the minimization of its negative.
#'
#' @keywords optimize
#' @template family_pareto_dominance_checks
#' @template family_multi_objective_tools
#'
#' @param x [\code{numeric}]\cr
#'   First vector.
#' @param y [\code{numeric}]\cr
#'   Second vector.
#' @return Single logical value.
#' @rdname dominates
#' @export
#' @examples
#' dominates(c(2, 3), c(4, 5))
#' dominates(c(2, 3), c(2, 3))
#' is_dominated(c(2, 3), c(4, 5))
#' is_dominated(c(4, 5), c(2, 3))
#' c(1, 2, 3) %dominates% c(4, 5, 6)
#' c(1, 2, 3) %is_dominated% c(4, 5, 6)
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
#' Check for Pareto-dominance
#'
#' @description
#' These functions take a numeric matrix as input where each column corresponds to
#' a point and return a logical vector. The \eqn{i}th position of the latter is
#' \code{TRUE} if the \eqn{i}th point is dominated by at least one other point for
#' \code{dominated} and \code{FALSE} for \code{nondominated}.
#'
#' @keywords optimize
#' @template family_pareto_dominance_checks
#' @template family_multi_objective_tools
#'
#' @param x [\code{matrix}]\cr
#'   Numeric \eqn{(m \times n)} matrix where \eqn{m} is the number of objectives
#'   and \eqn{n} is the number of points.
#' @return Logical vector where the \eqn{i}th component is \code{TRUE} if
#'   the point is dominated or nondominated respectively.
#' @export
#' @rdname dominated
#' @examples
#' x = matrix(c(1, 1, 2, 2, 1, 3), byrow = FALSE, ncol = 3L)
#' dominated(x)
#' nondominated(x)
dominated = function(x) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  return(.Call("dominated_c", x))
}

#' @rdname dominated
#' @export
nondominated = function(x) {
  return(!dominated(x))
}

#' @title D
#' etermine which points of a set are (non)dominated
#'
#' @description
#' Given a matrix with one point per column \code{which_dominated} returns the
#' column numbers of the dominated points and \code{which_nondominated} the column
#' numbers of the nondominated points. Function \code{is_maximally_dominated} returns
#' a logical vector with \code{TRUE} for each point which is located on the last
#' non-domination level.
#'
#' @keywords optimize
#' @template family_pareto_dominance_checks
#' @template family_multi_objective_tools
#'
#' @param x [\code{matrix}]\cr
#'   Numeric \eqn{(m \times n)} matrix where \eqn{n} is the number of points and \eqn{m}
#'   is the number of objectives.
#' @return Integer vector of positions of (non)dominated points.
#' @rdname which_dominated
#' @export
#' @examples
#' data(mtcars)
#' # assume we want to maximize horsepower and minimize gas consumption
#' cars = mtcars[, c("mpg", "hp")]
#' cars$hp = -cars$hp
#' idxs = which_nondominated(t(as.matrix(cars)))
#' \dontrun{
#' print(mtcars[idxs, ])
#' plot(cars)
#' points(cars[idxs, ], col = "tomato", pch = 10)
#' }
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
#' Check if one set Pareto-dominates another
#'
#' @description
#' The function checks, whether every point from the second set of points
#' is dominated by at least one point from the first set.
#'
#' @template family_pareto_dominance_checks
#' @template family_multi_objective_tools
#'
#' @param x [\code{matrix}]\cr
#'   First set of points.
#' @param y [\code{matrix}]\cr
#'   Second set of points.
#' @return Single logical value.
#' @export
#' @examples
#' x = matrix(c(1, 1, 2, 2, 1, 3), byrow = FALSE, ncol = 3L)
#' y = matrix(c(3, 4, 2, 2, 5, 6, 7, 7), byrow = FALSE, ncol = 4L)
#' set_dominates(x, y)
#' set_dominates(y, x)
set_dominates = function(x, y) {
  n1 = ncol(x)
  n2 = ncol(y)
  z = cbind(x, y)

  dom = dominated(z)
  all(dom[(n1 + 1L):(n1 + n2)])
}
