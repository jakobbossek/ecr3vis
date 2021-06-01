#' @title
#' R1, R2 and R3-indicator
#'
#' @description
#' Consider two Pareto-front (approximation) sets
#' \eqn{X = \{x_1, \ldots, x_{|X|}\}} and \eqn{Y = \{y_1, \ldots, y_{|Y|}\}} for
#' some optimization problem with \eqn{m} objectives. Furthermore, let \eqn{U} be
#' a set of \emph{utility functions}
#' each of the form \eqn{u : R^m \to R} which maps each point in the objective
#' space into a so-called measure of utility. Let \eqn{p} be a probability
#' distribution over \eqn{U}. We denote by \eqn{u^{*}(A) = \max_{x \in A} u(x)}
#' the maximum value reached by some utility function \eqn{u} on a set \eqn{A}.
#'
#' The R1-indicator measures the probability that approximation set \eqn{X} is
#' better than approximation set \eqn{Y} by integrating over the all utility
#' function \eqn{u \in U}. More precisely, it is defined as
#' \deqn{
#'   R1(X, Y, U, p) = \int_{u \in U} C(X, Y, u)p(x)du
#' }
#' where \eqn{C(X, Y, u) = 1} if \eqn{u^{*}(X) > u^{*}(Y)}, \eqn{C(X, Y, u) = 1/2}
#' if \eqn{u^{*}(X)=u^{*}(Y)} and \eqn{C(X, Y, u) = 0} otherwise is termed the
#' \emph{outcome function}.
#'
#' Interpretation: if \eqn{R1(X, Y, U, p) > 0.5}, \eqn{X} is better than \eqn{Y}.
#' Owing to the definition it holds that
#' \deqn{
#'   R1(X, Y, U, p) = 1 - R1(X, Y, U, p)
#' }
#' and therefore \eqn{X} and \eqn{Y} cannot outperform each other simultaneously.
#'
#' In practice this defintion is not of much use. Therefore, a finite discrete
#' set of utility functions is used with the uniform probability distribution.
#' Then, \eqn{R1} can be written as
#' \deqn{
#'   R1(X, Y) = \frac{1}{|U|}\sum_{u \in U} C(X, Y, u).
#' }
#' The same approach is done for \eqn{R2} and \eqn{R3} which will be discussed in the
#' following.
#'
#' Classical utility functions involve \emph{weighted sum}, \emph{Tschebycheff} and \emph{augmented
#' Tschbycheff} all of which are availabe via the argument \code{utility} in the
#' implementation. As an example, the set of (weighted) Tschebycheff utility
#' functions is \eqn{U_{\infty} = (u_{\lambda})} where
#' \deqn{
#'   u_{\lambda}(x) = -\max_{j=1, \ldots, m} \left(\lambda_j \cdot |x_j - r_j|\right)
#' }
#' where \eqn{r \in R^m} is the ideal point or an approximation thereof and
#' \eqn{\lambda \in R^m} is a weight vector with \eqn{\lambda_j \geq 0} for \eqn{j=1,\ldots,m}
#' and \eqn{\sum_{j=1}^{m} \lambda_j = 1}. Further details are beyond the scope
#' of this documentation (see [1] for in-depth information).
#'
#' The R2-indicator instead considers the expected utility values. It is defined as
#' \deqn{
#'   R2(X, Y, U, p) = E(u^{*}(X)) - E(u^{*}(Y)) = \int_{u \in U} (u^{*}(A) - u^{*}(B))p(u)du.
#' }
#' Interpretation: here, since the measure is defined as the difference of
#' expectations, approximation set \eqn{X} is considered better than set \eqn{Y}
#' if \eqn{R(X, Y) > 0}.
#'
#' Eventually, indicator R3 considers the ratios of best utility function values:
#' \deqn{
#'   R3(X, Y, U, p) = E\left(\frac{u^{*}(Y) - u^{*}(X)}{u^{*}(Y)}\right) = \int_{u \in U} \frac{u^{*}(Y) - u^{*}(X)}{u^{*}(Y)}p(u)du.
#' }
#' For further details we refer the reader to Hansen and Jaszkiewicz [1].
#'
#' @references
#' [1] M. P. Hansen and A. Jaszkiewicz. 1998. Evaluating the quality of
#' approximations to the nondominated set. Imm-rep-1998-7. Institute of
#' Mathematical Modeling, Technical University of Denmark.
#'
#' @param x [\code{matrix}]\cr
#'   First point set in column major format.
#' @param y [\code{matrix}]\cr
#'   Second point set in column major format.
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
#'   Default is \dQuote{tschebycheff}.
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
