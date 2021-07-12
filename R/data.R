#' @title
#' Exemplary Pareto-front approximations sets
#'
#' @description
#' A dataset containing Pareto-front approximation sets for the continuous
#' bi-objective (i.e., \eqn{m=2}) problems ZDT1 and ZDT3 (see [3]). The data
#' was calculated by the Evolutionary Multi-Objective Algorithms (EMOAS)
#' NSGA-II [1] and SMS-EMOA [2].
#'
#' @references
#' [1] Deb, K., Pratap, A., and Agarwal, S. A Fast and Elitist Multiobjective Genetic
#' Algorithm: NSGA-II. IEEE Transactions on Evolutionary Computation, 6 (8) (2002),
#' 182-197.
#'
#' [2] Beume, N., Naujoks, B. and Emmerich, M. SMS-EMOA: Multiobjective selection
#' based on dominated hypervolume. European Journal of Operational Research, 2007,
#' vol. 181, issue 3, 1653-1669.
#'
#' [3] E. Zitzler, K. Deb, L. Thiele, Comparison of multiobjective evolutionary
#' algorithms: Empirical results, Evolutionary computation 8 (2000) 173–195.
#'
#' @format A data frame with 5 columns/variables: \code{y1} and \code{y2}
#' (objective function values), \code{algorithm}, \code{problem} and \code{repl}
#' (independent replication due to the stochsticity of the algorithms).
"emoas_on_zdt"
