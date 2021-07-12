#' ecr3vis: Evolutionary Computation in R (v3.x.x)
#'
#' The package is a framework/toolbox for the rapid implementation of
#' \emph{Evolutionary / Genetic Algorithms} for solving complex optimization
#' problems and for the analysis of such algoritms' performances.
#'
#' @section Evolutionary Algorithms (EAs):
#' \emph{Evolutionary Computation} is an umbrella term for randomized search heuristics
#' that draw inspiration from natural evolution. There are four historic strands that
#' are nowdays known as \emph{Evolutionary Algorithms}: Evolutionary Programming,
#' Evolution Strategies, Genetic Algorithms and Genetic Programming. All four
#' flavors emerged in the 1980s mainly in the area of engineering and differ
#' with respect to the considered decision space, operators and
#' algorithmic parameters.
#'
#' Evolutionary Algorithms maintain a so-called \emph{population} of
#' \emph{(candidate) solutions} or \emph{individuals} that is often initialized
#' with initial solutions sampled randomly from the decision space. Next,
#' in a loop the algorithms basically (i) select some individuals as parents
#' (ii) generate new individuals from the parents by applying bio-inspired
#' random operators like recombination/crossover and/or mutation and (iii) in
#' a second round of selection, choose a subset of parents and/or offspring
#' individuls to \dQuote{survive} and form the next population. In this last step,
#' \dQuote{fitter} individuals have a higher probability to survive
#' (analogy to the \emph{survival of the fittest} principle from Darwin's evolution
#' theory). This process is repeated until some kind of stopping condition is met.
#' Basically, all EAs fit into this general theme. There exists a plethora of variation
#' operators, representations and ideas in this field of research. Despite this
#' rather simple structure, EAs have been successfully applied to countless
#' (very) complex (often multi-objective) optimization problems with major success.
#' One of the benefits of EAs is that they are often easy to implement.
#'
#' @section Implementing EAs with ecr3vis:
#' This package offers many building blocks for rapid prototyping of algorithmic
#' ideas: fast C(++)-based implementations of well-known crossover and mutation
#' operators for standard representations (binary, permutation, real-valued),
#' various selection operators (uniform random, \eqn{k}-tournament, fitness-
#' proportionate etc.) and various utility functions. The user can easily set up
#' custom objective functions, operators, building blocks, and representations
#' sticking to few conventions. The package allows both a black-box approach for
#' standard tasks (plug-and-play style) and a much more flexible approach where
#' the evolutionary cycle is written by hand. The package excells with support
#' for arbitrary complex custom representations, easy parallelization
#' and much more.
#'
#' @section Performance Assessment and Visualization:
#' The package offers a rich set of functions for the assessment of performance
#' of single algorithms and the comparison of multiple algorithms. Here, the
#' of focus lies on performance measurement of multi-objective randomized search
#' heuristics where the comparison of approximation sets with incomparable
#' solutions is much more challenging than in the single-objective setting.
#'
#' @subsection Multi-objective performance indicators
#' So-called quality indicators map an approximation set \eqn{X} to the real
#' numbers. Our package offers the most relevant multi-objective performance
#' indicators among others (see reference [1] for an excellent tutorial):
#' \itemize{
#'   \item Hypervolume / S-metric (HV),
#'   \item (Inverted) Generational Distance (GD, IGD, IGD+),
#'   \item \eqn{\varepsilon}-indicator,
#'   \item R-indicator family (R1, R2 and R3),
#'   \item Coverage / C-metric,
#'   \item Riesz \eqn{s}-energy.
#' }
#'
#' @subsection Multi-objective set visualization
#' Visualizing solution sets calculated by Evolutionary Multi-Objective Algorithms
#' (EMOAs) is crucial since humans are usually better in grasping information
#' from a (good) graphic rather than large tables of numbers [2]. Currently,
#' the package offers the following plot types:
#' \itemize{
#'   \item 2D/3D Scatterplots,
#'   \item Empirical Attainment Functions (EAF),
#'   \item Parallel Coordinate Plots (PCP),
#'   \item Heatmaps,
#'   \item Radar/Spider Plots.
#' }
#'
#' @section Parallelization:
#' In this package some functions support parallelization for faster execution via
#' the package \CRANpkg{future}. A parallel backend (e.g., multicore (on Unix/Linux/MacOS),
#' multisession etc.) can be selected via \code{\link[future]{plan}}.
#'
#' @section Suggested packages:
#' \itemize{
#'   \item Parallelization framework: \CRANpkg{future}
#'   \item 3D plots with one of: \CRANpkg{plot3D}, \CRANpkg{plot3Drgl}, \CRANpkg{scatterplot3d} or \CRANpkg{plotly}
#'   \item Empirical Attainment Functions: \CRANpkg{eaf}
#' }
#'
#' @references
#' [1] Knowles, J. D., Thiele, L. and Zitzler, E. A tutorial on the performance
#' assessment of stochastive multiobjective optimizers. TIK-Report No. 214, Computer
#' Engineering and Networks Laboratory, ETH Zurich, February 2006 (Revised version.
#' First version, January 2005). doi: 10.3929/ethz-b-000023822
#'
#' [2] T. Tušar and B. Filipič, Visualization of Pareto Front Approximations in
#' Evolutionary Multiobjective Optimization: A Critical Review and the Prosection
#' Method, in IEEE Transactions on Evolutionary Computation, vol. 19, no. 2,
#' pp. 225-245, April 2015, doi: 10.1109/TEVC.2014.2313407
#'
#' @docType package
#' @name ecr3vis-package
#' @rdname ecr3vis-package
NULL
