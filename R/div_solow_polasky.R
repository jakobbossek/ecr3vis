#' @title
#' Solow-Polasky measure
#'
#' @description
#' Calculates the Solow-Polasky measure [1] for a set of points given as columns
#' of a numeric matrix.
#'
#' @details
#' This measure was introduced by Solow and Polasky back in 1994 to measure the
#' amount of diversity between species in biology [1]. Later, Ulrich and Thiele [2]
#' adopted this measures for \emph{Evolutionary Diversity Optimization} where the
#' goal is to come up with a population \eqn{P = \{P_1, \ldots, P_{\mu}\}} of \eqn{\mu}
#' individuals such that the following holds:
#' \enumerate{
#'   \item All individuals in \eqn{P} adhere to a minimum quality
#' threhold, i.e., \eqn{f(x) \leq v_{\min}} for some threhold value \eqn{v_{\min}}
#' (we assume the fitness function \eqn{f} w.l.o.g. to be minimized).
#'   \item The population should be \dQuote{diverse} with respect to some diversity
#'   measure \eqn{D} that maps the population to a single scalar numeric value.
#' }
#' Given \eqn{P = \{P_1, \ldots, P_{\mu}\}} and pairwise distances \eqn{d(P_i, P_j)}
#' \eqn{1 \leq, i,j \leq \mu} let \eqn{M} be a \eqn{(\mu \times \mu)} matrix
#' with
#' \deqn{
#'   M_{ij} = \exp(-\theta \cdot d(P_i, P_j)).
#' }
#' Then the Solow-Polasky diversity is defined as
#' \deqn{
#'   D_{SP}(P) = \sum_{1 \leq i,j \leq \mu} M_{ij}^{-1} \in [1, \mu]
#' }
#' where matrix \eqn{M^{-1}} is the Moore-Penrose generalized inverse of a
#' matrix \eqn{M}.
#' \eqn{D_{SP}} can be interpreted as \dQuote{as the number of different species in
#' the population} [2]. Note however that the measure calculates a real valued
#' diversty in \eqn{[1, \mu]} and no integer value. Hence, it can be seen as
#' a more fine-grained measure of diversity that captures a distance
#' other than the \dQuote{binary} distance \eqn{d'} were \eqn{d'(P_i, P_j) = 1}
#' if \eqn{P_i \neq P_j} and \eqn{d'(P_i, P_j) = 0} otherwise.
#'
#' The runtime is dominated by the inverse matrix calculation and hence is upper
#' bounded by \eqn{O(\mu^3)}. In [2], the authors propose an alternative method
#' to update the calculation once the population changes. However, the measure
#' implemented here is meant to be used \emph{a-posteriori} to assess the
#' \dQuote{diversity} of a final population.
#'
#' @references
#' [1] Andrew R. Solow and Stephen Polasky. Measuring biological diversity.
#' English (US). In: Environmental and Ecological Statistics 1.2 (June 1994),
#' pp. 95–103. issn: 1352-8505. doi: 10.1007/BF02426650.
#'
#' [2] Tamara Ulrich and Lothar Thiele. Maximizing population diversity in
#' single-objective optimization. In: 13th Annual Genetic and Evolutionary
#' Computation Conference (GECCO 2011). ACM, 2011, pp. 641–648.
#' doi: 10.1145/2001576.2001665.
#'
#' @template family_diversity_indicators
#'
#' @param x [\code{matrix}]\cr
#'   Numeric input matrix (e.g., a EA-population) where each column holds one point / individual.
#' @param d [\code{matrix} | \code{NULL}]\cr
#'   Optional distance matrix.
#'   If \code{NULL}, the default, the Euclidean distance is calculated
#'   automatically via \code{\link[stats]{dist}}.
#' @param theta [\code{numeric(1)}]\cr
#'   Single scalar value (see details section).
#'   Default is 1.
#' @param ... [any]\cr
#'   Further argument passed down to \code{\link[stats]{dist}}
#'   in case \code{d} is \code{NULL}.
#' @return Solow-Polasky diversity measure (scalar numeric value).
#' @export
#' @examples
#' # Generate a random point cloud in [0,1] x [0,1]
#' x = matrix(runif(100), nrow = 2L)
#' solow_polasky(x)
#' solow_polasky(x, theta = 2)
#'
#' # All points equal
#' x = matrix(rep(1, 100), nrow = 2L)
#' solow_polasky(x)
solow_polasky = function(x, d = NULL, theta = 1, ...) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    re::stopf("[solow_polasky] Package \"MASS\" needed for this function to work.")
  }
  checkmate::assert_matrix(x, min.rows = 1L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_number(theta, lower = 0.0001)
  nc = ncol(x)
  if (!is.null(d)) {
    if (!checkmate::check_matrix(d, mode = "numeric", nrows = nc, ncols = nc, any.missing = FALSE, all.missing = FALSE)) {
      re::stopf("[solow_polasky] Argument d needs to be a (%i x %i) matrix where n is the number
        of points in the point set x.", nc)
    }
    if (!all(d == t(d)))
      re::stopf("[solow_polasky] Matrix d, as a distance matrix, must be symmetric!")
  }

  if (is.null(d)) {
    # NOTE: need to transpose here since all ecr3vis functions expect
    # a matrix in "column-major format"
    d = as.matrix(dist(t(x), ...))
  }

  # calculate Solow-Polasky
  M = exp((-1) * theta * d)
  M = MASS::ginv(M)
  return(sum(M))
}
