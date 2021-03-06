#' @title
#' Reference point approximation
#'
#' @description
#' Helper functions to compute nadir or ideal point from multiple sets of
#' points, e.g., multiple approximation sets produced by some multi-objective
#' evolutionary algorithm.
#'
#' @param ... [\code{matrix}]\cr
#'   Arbirary number of matrizes.
#' @param sets [\code{list}]\cr
#'   List of matrizes. This is an alternative way of passing the sets. Can be used
#'   exclusively or combined with \code{...}.
#' @return Reference point (numeric vector).
#' @rdname reference_point_approximation
#' @family mootools
#' @export
get_nadir = function(..., sets = NULL) {
  get_point(..., sets = sets, FUN = max)
}

#' @export
#' @rdname reference_point_approximation
get_ideal = function(..., sets = NULL) {
  get_point(..., sets = sets, FUN = min)
}

# @title
# Helper to compute the nadir/ideal point.
#
# @description
# The functions expects a set of sets and a function FUN to apply.
#
# @param ... [any]\cr
#   Arbitrary number of matrizes.
# @param sets [\code{list}]\cr
#   List of sets. This is an alternative way of passing the sets. Can be used
#   exclusively or combined with \code{...}.
# @param FUN [\code{function}]\cr
#   Either min or max function.
# @return [\code{numeric}]
get_point = function(..., sets = NULL, FUN) {
  checkmate::assert_function(FUN)

  # we can combine both types of parameter passing here
  sets2 = list(...)
  if (!is.null(sets)) {
    checkmate::assert_list(sets, types = "matrix")
  }
  sets = c(sets, sets2)
  assert_list_of_point_sets(sets)

  # sapply returns a matrix with each row corresponding to the nadir point
  # of a single set. We hence apply the FUN rowwise again.
  apply(sapply(sets, function(set) {
    apply(set, 1L, FUN)
  }), 1L, FUN)
}

# @title
# Checks for point set.
#
# @description
# Helper function to check if all given sets have the same dimension, i.e.,
# number of objectives.
#
# @param x [list]
#   List of sets, i.e., matrizes.
assert_list_of_point_sets = function(x) {
  checkmate::assert_list(x, types = "matrix")
  n.rows = sapply(x, nrow)
  if (length(unique(n.rows)) > 1L) {
    re::stopf("All sets need to be of the same dimension.")
  }
}
