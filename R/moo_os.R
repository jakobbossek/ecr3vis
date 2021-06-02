#' @title
#' Overall (Pareto) spread
#'
#' @description
#' A simple multi-objective performance indicator proposed in [1].
#' Given a point set\eqn{X = \{x_1, \ldots, x_{|X|}\}} of \eqn{m} objectives,
#' the ideal point \eqn{I} (or an approximation) and the nadir point \eqn{N}
#' (or a approximation) it is defined as
#' \deqn{
#'   OS(X) = \prod_{i=1}^{m} \frac{\max_{x \in X} x_i - \min_{x \in X} x_i}{N_i - I_i}.
#' }
#' This indicator only captures the extend of the Pareto-front approximation.
#'
#' @references
#' [1] J. Wu, S. Azarm, Metrics for quality assessment of a multiobjective design
#' optimization solution set, Journal of Mechanical Design 123 (1) (2001) 18–25.
#'
#' @param x [\code{matrix}]\cr
#'   Point set in column major format.
#' @param np [\code{numeric}]\cr
#'   Nadir point or approximtion of it.
#' @param ip [\code{numeric}]\cr
#'   Ideal point or approximtion of it.
#' @template arg_dots_not_used
#' @return Single numeric indicator value.
#'
#' @keywords optimize
#' @template family_multi_objective_performance_indicators
#' @export
os = function(x, np = NULL, ip = NULL, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_numeric(np, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  checkmate::assert_numeric(ip, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  if (is.null(np))
    np = get_nadir(x)
  if (is.null(ip))
    ip = get_ideal(x)

  if ((nrow(x) != length(np)) | (nrow(x) != length(ip)))
    re::stopf("[os] length of nadir and ideal points must match number of rows of x.")

  .Call("_ecr3vis_os_c", x, np, ip)
}
