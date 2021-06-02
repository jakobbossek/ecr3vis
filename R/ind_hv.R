#' @title
#' Dominated hypervolume (contribution)
#'
#' @description
#' The \emph{hypervolume (HV) indicator} [1, 2] is arguebly one of the most often used performance
#' indicator likely due to its straight-forward definition. Given a set of points
#' \eqn{X = \{x_1, \ldots, x_{|X|}\}} and an anti-optimal reference point
#' \eqn{r \in R^m} the \emph{HV-indicator}, also called the \emph{S-metric}, is defined
#' as
#' \deqn{
#'   HV(X, r) = \lambda_m\left(\bigcup_{x \in X} [x \preceq x' \preceq r]\right).
#' }
#' Here, \eqn{\lambda_m} is the \eqn{m}-dimensional Lebesgue measure. Informally,
#' the hypervolume indicator is the space/volume enclosed by the point set and
#' the anti-optimal (i.e., it is dominated by every point \eqn{x \in X}) reference
#' point. It is known to be strictly monotonic. I.e., if point set \eqn{X} strictly
#' dominates another point set \eqn{Y}, then \eqn{HV(X,r) > HV(Y, r)} holds.
#'
#' Function \code{hv} computes the dominated hypervolume of a set of points
#' given a reference set whereby \code{hv_contr} computes the hypervolume contribution
#' of each point which is a key ingredient of the S-Metric Selection EMOA
#' (SMS-EMOA) [3, 4].
#'
#' If no reference point is given the nadir point of the set \code{x} is
#' determined and a positive offset with default 1 is added. This is to ensure
#' that the reference point is dominated by all of the points in the reference set
#' (anti-optimality property).
#'
#' @note
#' Keep in mind that this function assumes all objectives to be minimized.
#' In case at least one objective is to be maximized the matrix \code{x} needs
#' to be transformed accordingly in advance.
#'
#' @references
#' [1] E. Zitzler and L. Thiele. Multiobjective Optimization Using Evolutionary
#' Algorithms - A Comparative Case Study. In Conference on Parallel Problem Solving
#' from Nature (PPSN V), volume 1498 of LNCS, pages 292–301, 1998.
#'
#' [2] Knowles, J. D., Thiele, L. and Zitzler, E. A tutorial on the performance
#' assessment of stochastive multiobjective optimizers. TIK-Report No. 214, Computer
#' Engineering and Networks Laboratory, ETH Zurich, February 2006 (Revised version.
#' First version, January 2005). doi: 10.3929/ethz-b-000023822
#'
#' [3] M. Emmerich, N. Beume, and B. Naujoks. An EMO Algorithm Using the Hypervolume
#' Measure as Selection Criterion. In Conference on Evolutionary Multi-Criterion
#' Optimization (EMO 2005), volume 3410 of LNCS, pages 62–76. Springer Berlin, 2005.
#'
#' [4] Beume, N., Naujoks, B. and Emmerich, M. SMS-EMOA: Multiobjective selection
#' based on dominated hypervolume. European Journal of Operational Research, 2007,
#' vol. 181, issue 3, 1653-1669.
#'
#' @param x [\code{matrix}]\cr
#'   Matrix of points (column-wise).
#' @param r [\code{numeric} | \code{NULL}]\cr
#'   Reference point.
#'   Set to the maximum in each dimension by default if not provided.
#' @param offset [\code{numeric(1)}]\cr
#'   Offset to be added to each component of the reference point only in the case
#'   where no reference is provided and one is calculated automatically.
#'   Default is 1.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return Dominated hypervolume in the case of
#'  \code{hv} or a vector the dominated hypervolume contributions
#'  for each point of \code{x} in the case of \code{hv_contr}.
#' @rdname hypervolume
#' @template family_multi_objective_performance_indicators
#' @export
hv = function(x, r = NULL, offset = 1, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)

  if (is.null(r)) {
    r = get_nadir(x) + offset
  }

  if (any(is.infinite(x))) {
    re::warningf("Set of points contains %i infinite values.", sum(is.infinite(x)))
    return(NaN)
  }

  if (length(r) != nrow(x)) {
    re::stopf("Set of points and reference point need to have the same dimension, but
      set of points has dimension %i and reference point has dimension %i.", nrow(x), length(r))
  }

  if (any(is.infinite(r))) {
    re::warningf("Reference point contains %i infinite values.", sum(is.infinite(r)))
    return(NaN)
  }

  return(.Call("hv_c", x, r))
}

#' @export
#' @rdname hypervolume
hv_contr = function(x, r = NULL, offset = 1) {
  if (is.null(r)) {
    r = get_nadir(x) + offset
  }
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_numeric(r, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_number(offset, finite = TRUE, lower = 0)

  # NOTE: we pass  a copy of x here, since otherwise the C-code changes x in place
  return(.Call("hv_contr_c", x[,], r))
}
