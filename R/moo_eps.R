#' @title
#' Binary \eqn{\varepsilon}-indicator
#'
#' @description
#' For two point sets \code{x} and \code{y} the function returns the value
#' of the binary \eqn{\varepsilon}-indicator [1].
#'
#' @details
#' The \eqn{\varepsilon}-indicator, often denoted as \eqn{I_{\varepsilon}},
#' requires the concept of \eqn{\varepsilon}-dominance.
#' A vector \eqn{x \in R^m}, for some \eqn{\varepsilon > 0}, \eqn{\varepsilon}-dominates
#' another vector \eqn{y \in R^m}, \eqn{x \preceq_{\varepsilon} y}, if and only if
#' \deqn{
#'   \Leftrightarrow x_i \leq \varepsilon y_i \quad \forall i = 1,\ldots,m.
#' }
#' Equipped with this, Zitzler et al. [1] define the (multiplicative) binary
#' \eqn{\varepsilon}-indicator for two point sets \eqn{X = \{x_1, \ldots, x_{|X|}\}}
#' and \eqn{Y = \{y_1, \ldots, y_{|Y|}\}} as follows:
#' \deqn{
#'   I_{\varepsilon}(X, Y) = \inf_{\varepsilon > 0}\{y \in Y \mid \exists x \in X: x \preceq_{\varepsilon} y\}.
#' }
#' It means that \eqn{I_{\varepsilon}(X, Y)} is the smallest \eqn{\varepsilon}
#' such that there exists a point in \eqn{X} that dominates a point \eqn{y \in Y}
#' in the \eqn{\varepsilon}-dominance sense. It can be calculated the following way
#' \deqn{
#'   I_{\varepsilon}(X, Y) = \max_{y \in Y} \min_{x \in X} \max_{1 \le i \le m} \frac{x_i}{y_i}.
#' }
#' Given a reference set \eqn{R}, e.g., the known true Pareto-front or a good
#' approximation of it, the unary version is simply
#' \deqn{
#'   I_{\varepsilon}(X) := I_{\varepsilon}(R, X).
#' }
#' Function \code{eps} implements the binary \eqn{\varepsilon}-indicator. It
#' should be obvious how to calculate the unary indicator.
#'
#' @template note_minimization
#'
#' @references
#' [1] E. Zitzler, L. Thiele, M. Laumanns, C. M. Fonseca, V. G. Da Fonseca,
#' Performance assessment of multiobjective optimizers: An analysis and review,
#' IEEE Transactions on evolutionary computation 7 (2) (2003) 117–132.
#'
#' @keywords optimize
#' @template family_multi_objective_performance_indicators
#'
#' @param x [\code{matrix}]\cr
#'   First numeric matrix of points (each colum contains one point).
#' @param y [\code{matrix}]\cr
#'   Second numeric matrix of points (each colum contains one point).
#' @template arg_dots_not_used
#' @return Single numeric indicator value.
#' @export
eps = function(x, y, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(y, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
  if (nrow(x) != nrow(y))
    re::stopf("[eps] Point sets x and y must have the same number of rows.")

  return(.Call("eps_c", x, y))
}
