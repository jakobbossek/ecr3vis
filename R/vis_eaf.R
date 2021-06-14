#' @title
#' Empirical attainment function (EAF)
#'
#' @description
#' Plots the Empiriccal Attainment Function (EAF) given approximation sets of
#' multi-objective stochastic algorithms.
#'
#' @description
#' The following description closely follows the excellent introduction into
#' performance assessment of multi-objective optimizers by Knowles et al. [3].
#' (Evolutionary) multi-objective algorithms are stochastic. Hence, naturally,
#' the result produced by such an algorithm can be described by a probability
#' distribution. This distribution can be descibed by a random set
#' \deqn{
#'   Z = \{z^j \in R^m \,|\, j = 1, \ldots, |Z|\}
#' }
#' where the size \eqn{|Z|} is also random. The \emph{attainment function}
#' \eqn{\alpha_Z : R^m \to [0,1]} is defined as
#' \deqn{
#'   \alpha_Z(z)
#'   = P(Z \preceq \{z\}) = P\left(z^1 \preceq z \lor \ldots \lor z^{|Z|} \preceq z\right).
#' }
#' where \eqn{z^j \preceq z} denotes that \eqn{z^j} weakly dominates \eqn{z}.
#' This can be interpreted as the \emph{probability to reach goal} \eqn{z} in
#' the sense that there is at least one objective vector in the solution set
#' that weakly dominates, i.e., \dQuote{attains}, \eqn{z}.
#'
#' Given \eqn{r} independent runs of a stochastic multi-objective optimizer and
#' denoting by \eqn{X^i, 1 \leq i \leq r} the corresponding Pareto-front
#' approximation set the \emph{empirical attainment function (EAF)} is defined
#' as
#' \deqn{
#'   \hat{\alpha}_r(z) = \frac{1}{r} \sum_{i=1}^{r} I(X^i \preceq \{z\})
#' }
#' where \eqn{I(\cdot)} is the \emph{indictor function} evaluating to \eqn{1}
#' if and only if the condition is true; \eqn{0} otherwise.
#' This is a straight-forward estimation from empirical data and simply describes
#' the frequency of attaining \eqn{z} within \eqn{r} runs.
#'
#' The EAF can be used for visualization. Here, we plot the so-called
#' \eqn{k\%}-attainment surface which splits \dQuote{the goals that have been attained
#' and the goals that have not been attained with a frequency of at least
#' \eqn{k} percent} [3].
#'
#' For further details we refer the reader to the technical report by Knowles,
#' Thiele and Zitzler (reference [3]).
#'
#' @template family_multi_objective_visualizations
#'
#' @references
#' [1] V. Grunert da Fonseca and C. M. Fonseca, The attainment-function approach
#' to stochastic multiobjective optimizer assessment and comparison, in
#' Experimental Methods for the Analysis of Optimization Algorithms
#' (T. Bartz-Beielstein, M. Chiarandini, L. Paquete, and M. Preuss, eds.),
#' ch. 5, pp. 103-130, Springer Berlin Heidelberg, 2010.
#'
#' [2] Manuel López-Ibáñez, Luís Paquete, and Thomas Stützle. Exploratory Analysis
#' of Stochastic Local Search Algorithms in Biobjective Optimization. In T.
#' Bartz-Beielstein, M. Chiarandini, L. Paquete, and M. Preuss, editors,
#' Experimental Methods for the Analysis of Optimization Algorithms,
#' pages 209–222. Springer, Berlin, Germany, 2010. doi: 10.1007/978-3-642-02538-9_9
#'
#' [3] Knowles, J. D., Thiele, L. and Zitzler, E. A tutorial on the performance
#' assessment of stochastive multiobjective optimizers. TIK-Report No. 214, Computer
#' Engineering and Networks Laboratory, ETH Zurich, February 2006 (Revised version.
#' First version, January 2005). doi: 10.3929/ethz-b-000023822
#'
#' @template arg_df
#' @template arg_obj_cols
#' @param percentiles [\code{numeric}]\cr
#'   Percentiles of the EAF that will be plotted as attainment surfaces.
#' @template return_ggplot
#' @export
#' @examples
#' \dontrun{
#' data(emoas_on_zdt)
#' plot_eaf(emoas_on_zdt, obj.cols = c("y1", "y2"))
#' plot_eaf(emoas_on_zdt[emoas_on_zdt$algorithm == "nsga2", ], obj.cols = c("y1", "y2"))
#' plot_eaf(emoas_on_zdt[emoas_on_zdt$algorithm == "nsga2", ], obj.cols = c("y1", "y2"), percentiles = c(50, 100))
#' }
plot_eaf = function(
  df,
  obj.cols,
  percentiles = c(0, 50, 75, 100)) {
  checkmate::assert_numeric(percentiles, lower = 0, upper = 100, min.len = 1L,
    any.missing = FALSE, all.missing = FALSE)

  if (!requireNamespace("eaf", quietly = TRUE))
    re::stopf("[plot_scatter3d] Package \"eaf\" needed for this function to work.")

  df = prepare_pf_for_visualization(df, obj.cols, n.obj = 2L)

  # Merge columns algorithm and problem
  df$grouping = paste0(as.character(df$problem), "-----", as.character(df$algorithm))

  dfeaf = eaf::eafs(points = df[, obj.cols], sets = df$repl, groups = df$grouping, percentiles = percentiles)
  colnames(dfeaf) = c("y1", "y2", "percentiles", "group")
  dfeaf$percentiles = factor(dfeaf$percentiles, levels = percentiles, ordered = TRUE)

  # Undo merging
  dfeaf = re::df_explode(dfeaf, split.col = "group", split = "-----", names =  c("problem", "algorithm"), keep = FALSE)

  n.algorithms = re::nunique(dfeaf$algorithm)
  n.problems = re::nunique(dfeaf$problem)

  g = ggplot2::ggplot(dfeaf, ggplot2::aes_string(x = "y1", y = "y2",
    linetype = "percentiles", colour = "percentiles"))
  g = g + ggplot2::geom_step()
  g = g + ggplot2::theme_minimal()
  g = g + ggplot2::labs(
    x = "y1",
    y = "y2",
    linetype = "Attainment surface",
    colour = "Attainment surface")
  g = g + ggplot2::scale_color_grey()
  g = g + ggplot2::theme(legend.position = "bottom")
  if (n.algorithms > 1L & n.problems > 1L)
    g = g + ggplot2::facet_wrap(algorithm ~ problem, scales = "free")
  else if (n.algorithms > 1L)
    g = g + ggplot2::facet_wrap(algorithm ~ ., scales = "free")
  else if (n.problems > 1L)
    g = g + ggplot2::facet_wrap(problem ~ ., scales = "free")
  return(g)
}
