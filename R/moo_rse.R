#' @title
#' Riesz \eqn{s}-energy
#'
#' @description
#' Calculate the Riesz \eqn{s}-energy measure for a set of points.
#'
#' @details
#' The Riesz \eqn{s}-energy is designed as a measure for the evenness of a set
#' of points \eqn{X = \{x_1, \ldots, x_{|X|}\}}. It is formally defined as
#' \deqn{
#'   R_s(X) = \sum_{x \in X} \sum_{y \in X, y \neq x} k_s(x, y)
#' }
#' where function
#' \deqn{
#'   k_s(x, y) = d(x, y)^{-s}, s > 0
#' }
#' and
#' \deqn{
#'   k_s(x, y) = -\log(d(x, y)), s = 0
#' }
#' is the so-called Riesz \eqn{s}-kernel and \eqn{d(x,y)} is the Euclidean
#' distance between \eqn{x} and \eqn{y}. The parameter \eqn{s \geq 0} steers
#' the desired degree of uniformity of the distribution with increasing
#' emphasis on uniformity for \eqn{s \to \infty}. See [1] for an application
#' of the Riesz \eqn{s}-energy in multi-objective evolutionary optimization
#' and [2, 3] for a mathematically rigorous introduction into the general idea.
#'
#' @references
#' [1] J. G. Falcón-Cardona, H. Ishibuchi and C. A. C. Coello, Riesz s-energy-based
#' Reference Sets for Multi-Objective optimization," 2020 IEEE Congress on
#' Evolutionary Computation (CEC), 2020, pp. 1-8, doi: 10.1109/CEC48606.2020.9185833.
#'
#' [2] D.P. Hardinand and E.B. Saff, Minimal Riesz energy point configurations for rectifiable
#' \eqn{d}-dimensional manifolds, Advances in Mathematics, vol. 193, no. 1, pp. 174–204, 2005.
#'
#' [3] D. P. Hardin and E. B. Saff, Discretizing Manifolds via Minimum Energy Points,
#' Notices of the AMS, vol. 51, no. 10, pp. 1186–1194, 2004.
#'
#' @keywords optimize
#' @template family_multi_objective_performance_indicators
#'
#' @param x [\code{matrix}]\cr
#'   Numeric matrix of points (each colum contains one point).
#' @param s [\code{numeric}]\cr
#'   Degree of uniformity \eqn{s \geq 0} (see description).
#' @template arg_dots_not_used
#' @return Single numeric indicator value.
#' @export
rse = function(x, s, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_number(s, lower = 0, finite = TRUE)

  .Call("_ecr3vis_rse_c", x, s)
}
