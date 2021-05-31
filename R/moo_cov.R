#' @title
#' Coverage / C-metrix of two (approximation) sets.
#'
#' @description
#' Given two (approximation) sets \eqn{X = \{x_1, \ldots, x_{|X|}\}}
#' and \eqn{Y = \{y_1, \ldots, y_{|Y|}\}} this indidcator calculates the fraction
#' of points from \eqn{Y} which are dominated by at least one point \eqn{x \in X}.
#' Formally it is defined as:
#' \deqn{
#'   C(X, Y) = \frac{|\{y \in Y \mid \exists x \in X with x \preceq y\}|}{|Y|}.
#' }
#' It holds that \eqn{C(X, Y) \in [0,1]}. Note that in general
#' \deqn{
#'   C(X, Y) \neq 1 - C(Y, X).
#' }
#'
#' @references
#' [1] E. Zitzler, Evolutionary algorithms for multiobjective optimization:
#' Methods and applications, Ph.D. thesis,  Swiss Federal Institute of
#' Technology Zurich (1999).
#'
#' @param x [\code{matrix}]\cr
#'   First point set in column major format.
#' @param y [\code{matrix}]\cr
#'   Second point set in column major format.
#' @template arg_dots_not_used
#' @return [\code{numeric(1)}] Coverage value.
#'
#' @keywords optimize
#' @family mootools
#' @family multi-objective performance indicators
#' @export
cov = function(x, y, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(y, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)

  if (nrow(x) != nrow(y))
    re::stopf("[cov] Both point sets must have the same dimension.")

  .Call("_ecr3vis_cov_c", x, y)
}
