#' @title
#' Functions for the calculation of the dominated hypervolume (contribution).
#'
#' @description
#' The function \code{hv} computes the dominated
#' hypervolume of a set of points given a reference set whereby
#' \code{hv_contr} computes the hypervolume contribution
#' of each point.
#'
#' If no reference point is given the nadir point of the set \code{x} is
#' determined and a positive offset with default 1 is added. This is to ensure
#' that the reference point dominates all of the points in the reference set.
#'
#' @note: Keep in mind that this function assumes all objectives to be minimized.
#' In case at least one objective is to be maximized the matrix \code{x} needs
#' to be transformed accordingly in advance.
#'
#' @param x [\code{matrix}]\cr
#'   Matrix of points (column-wise).
#' @param ref.point [\code{numeric} | \code{NULL}]\cr
#'   Reference point.
#'   Set to the maximum in each dimension by default if not provided.
#' @param offset [\code{numeric(1)}]\cr
#'   Offset to be added to each component of the reference point only in the case
#'   where no reference is provided and one is calculated automatically.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{numeric(1)}] Dominated hypervolume in the case of
#'  \code{hv} and the dominated hypervolume contributions
#'  for each point in the case of \code{hv_contr}.
#' @rdname hypervolume
#' @family mootools
#' @family multi-objective performance indicators
#' @export
#FIXME: add offset as in hv_contr?
hv = function(x, ref.point = NULL, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)

  if (is.null(ref.point)) {
    ref.point = apply(x, 1L, max)
  }

  if (any(is.infinite(x))) {
    re::warningf("Set of points contains %i infinite values.", sum(is.infinite(x)))
    return(NaN)
  }

  if (length(ref.point) != nrow(x)) {
    re::stopf("Set of points and reference point need to have the same dimension, but
      set of points has dimension %i and reference point has dimension %i.", nrow(x), length(ref.point))
  }

  if (any(is.infinite(ref.point))) {
    re::warningf("Reference point contains %i infinite values.", sum(is.infinite(ref.point)))
    return(NaN)
  }

  return(.Call("hv_c", x, ref.point))
}

#' @export
#' @rdname hypervolume
hv_contr = function(x, ref.point = NULL, offset = 1) {
  if (is.null(ref.point)) {
    ref.point = get_nadir(x) + offset
  }
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_numeric(ref.point, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_number(offset, finite = TRUE, lower = 0)

  # NOTE: we pass  a copy of x here, since otherwise the C-code changes x in place
  return(.Call("hv_contr_c", x[,], ref.point))
}
