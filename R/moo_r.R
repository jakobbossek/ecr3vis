#' @title
#' R{1,2,3}-indicators
#'
#' @description
#' ...
#'
#' @references
#' [1] M. P. Hansen and A. Jaszkiewicz. 1998. Evaluating the quality of
#' approximations to the nondominated set. Imm-rep-1998-7. Institute of
#' Mathematical Modeling, Technical University of Denmark.
#'
#' @param x [\code{matrix}]\cr
#'   Point set in column major format.
#' @param y [\code{matrix}]\cr
#'   Reference point set in column major format.
#' @param ip [\code{numeric}]\cr
#'   The utopia point of the true Pareto-front, i.e., each component of the point
#'   contains the best value if the other objectives are neglected.
#'   If \code{NULL} (default) the ideal point of the union of \code{x} and
#'   \code{y} is used.
#' @param np [\code{numeric}]\cr
#'   Nadir point of the true Pareto front or an approximation.
#'   If \code{NULL} (default) the nadir point of the union of \code{x} and
#'   \code{y} is used.
#' @param lambda [\code{integer(1)}]\cr
#'   Number of weight vectors to use in estimating the utility function.
#' @param utility [\code{character(1)}]\cr
#'   Name of the utility function to use. Must be one of \dQuote{weighted-sum},
#'   \dQuote{tschebycheff} or \dQuote{augmented-tschbycheff}.
#' @template arg_dots_not_used
#' @keywords optimize
#' @family mootools
#' @family multi-objective performance indicators
#' @rdname rindicator
#' @export
r1 = function(
  x, y,
  ip = NULL,
  np = NULL,
  lambda = NULL,
  utility = "tschebycheff",
  ...) {
  r(x, y, ip, np, lambda, utility, aggregator = function(ua, ur) mean(ua > ur) + mean(ua == ur) / 2, ...)
}

#' @rdname rindicator
#' @export
r2 = function(
  x, y,
  ip = NULL,
  np = NULL,
  lambda = NULL,
  utility = "tschebycheff",
  ...) {
  r(x, y, ip, np, lambda, utility, aggregator = function(ua, ur) mean(ur - ua), ...)
}

#' @rdname rindicator
#' @export
r3 = function(
  x, y,
  ip = NULL,
  np = NULL,
  lambda = NULL,
  utility = "tschebycheff",
  ...) {
  r(x, y, ip, np, lambda, utility, aggregator = function(ua, ur) mean((ur - ua) / ur), ...)
}

# @rdname emoa_indicators
r = function(
  x, y,
  ip = NULL, np = NULL,
  lambda = NULL,
  utility,
  aggregator,
  ...) {
  checkmate::assert_matrix(x, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(y, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  o = nrow(x)
  if (is.null(ip))
    ip = get_ideal(x, y)
  if (is.null(np))
    np = get_nadir(x, y)
  if (is.null(lambda))
    lambda = get_lambda(o)
  checkmate::assert_numeric(ip, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_numeric(np, any.missing = FALSE, all.missing = FALSE)
  utilities = c("weighted-sum", "tschebycheff", "augmented-tschbycheff")
  checkmate::assert_choice(utility, utilities)
  checkmate::assert_function(aggregator)

  # convert utility to integer index which is used by the C code
  utility = which((match.arg(utility, utilities)) == utilities)
  utility = as.integer(utility)

  lambda = checkmate::asInt(lambda)

  ind.x = .Call("r_c", x, ip, np, lambda, utility)
  ind.y = .Call("r_c", y, ip, np, lambda, utility)

  ind = aggregator(ind.x, ind.y)

  return(ind)
}

get_lambda = function(o) {
  if (o == 2L) {
    500L
  } else if (o == 3L) {
    30L
  } else if (o == 4L) {
    12L
  } else if (o == 5L) {
    8L
  } else {
    3L
  }
}
