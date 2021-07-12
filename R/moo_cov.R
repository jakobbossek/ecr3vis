#' @title
#' Coverage / C-metric of two (approximation) sets
#'
#' @description
#' Calculates the Coverage metric [1] (also known as the C-metric) given two sets
#' of points.
#'
#' @details
#' Given two (approximation) sets \eqn{X = \{x_1, \ldots, x_{|X|}\}}
#' and \eqn{Y = \{y_1, \ldots, y_{|Y|}\}} this indidcator calculates the fraction
#' of points from \eqn{Y} which are dominated by at least one point \eqn{x \in X}.
#' Formally, it is defined as:
#' \deqn{
#'   C(X, Y) = \frac{|\{y \in Y \mid \exists x \in X,\, x \preceq y\}|}{|Y|}.
#' }
#' It holds that \eqn{C(X, Y) \in [0,1]}. Note that in general \eqn{C(X, Y) \neq 1 - C(Y, X)}.
#'
#' @template note_minimization
#'
#' @references
#' [1] E. Zitzler, Evolutionary algorithms for multiobjective optimization:
#' Methods and applications, Ph.D. thesis,  Swiss Federal Institute of
#' Technology Zurich (1999).
#'
#' @keywords optimize
#' @template family_multi_objective_performance_indicators
#'
#' @param x [\code{matrix}]\cr
#'   First numeric matrix of points (each colum contains one point).
#' @param y [\code{matrix}]\cr
#'   Second numeric matrix of points (each colum contains one point).
#' @template arg_dots_not_used
#' @return Coverage value.
#' @export
cov = function(x, y, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(y, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)

  if (nrow(x) != nrow(y))
    re::stopf("[cov] Both point sets must have the same dimension.")

  .Call("_ecr3vis_cov_c", x, y)
}
