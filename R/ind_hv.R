#' @title
#' Dominated hypervolume (contribution).
#'
#' @description
#' The hypervolume (HV) indicator is arguebly one of the most often used performance
#' indicator likely due to its straight-forward definition. Given a set of points
#' \eqn{X = \{x_1, \ldots, x_{|X|}\}} and an anti-optimal reference point
#' \eqn{r \in R^m} the HV-indicator, also called the S-metric, is defined
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
#' of each point.
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
#' @return [\code{numeric(1)}] Dominated hypervolume in the case of
#'  \code{hv} and the dominated hypervolume contributions
#'  for each point in the case of \code{hv_contr}.
#' @rdname hypervolume
#' @family mootools
#' @family multi-objective performance indicators
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
