#' @title
#' Empirical attainment function (EAF) difference
#'
#' @description
#' Plot the differences between the empirical attainment functions (EAFs) of two
#' approximations sets side by side adopting \CRANpkg{ggplot2}. The left plot
#' shows the the values of the left EAF minus the right EAF and the right side
#' shows it the other way around.
#'
#' @note
#' If the given approxximation sets are huge, generating the plot may take some
#' time. Here, it is most often helpful to store the plot first in a file. Due
#' to the huge number of points the size of the file may be also very large. Here,
#' it is advisible to save the file in a image file format (PNG or JPEG) instead
#' of PDF.
#'
#' @note
#' This function re-implements partial functionality of \code{\link[eaf]{eafdiffplot}}
#' realizing the plotting with \CRANpkg{ggplot2} rather than base R plot functions.
#'
#' @template family_multi_objective_visualizations
#'
#' @seealso plot_eaf
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
#' @param df1 [\code{data.frame}]\cr
#'   First approximation set: data.frame with columns at least \code{obj.cols}
#'   and \dQuote{repl}. All other columns will be ignored!
#' @param df2 [\code{data.frame}]\cr
#'   First approximation set: data.frame with columns at least \code{obj.cols}
#'   and \dQuote{repl}. All other columns will be ignored!
#' @template arg_obj_cols
#' @param percentiles [\code{numeric}]\cr
#'   Percentiles of the EAF that will be plotted as attainment surfaces.
#' @param intervals [\code{integer(1)}]\cr
#'   The absolute range of the differences \eqn{[0, 1]} is partitioned into the
#'   number of intervals provided.
#'   Default is 5.
#' @template return_ggplot
#' @export
#' @examples
#' \dontrun{
#' data(emoas_on_zdt)
#' df1 = emoas_on_zdt[emoas_on_zdt$algorithm == "nsga2" & emoas_on_zdt$problem == "zdt1_2d_2o", ]
#' df2 = emoas_on_zdt[emoas_on_zdt$algorithm == "smsemoa" & emoas_on_zdt$problem == "zdt1_2d_2o", ]
#' plot_eaf_diff(df1, df2, obj.cols = c("y1", "y2"))
#' }
plot_eaf_diff = function(
  df1,
  df2,
  obj.cols,
  intervals = 5L,
  percentiles = c(0, 50, 75)) {
  intervals = checkmate::asInt(intervals, lower = 1L)
  checkmate::assert_numeric(percentiles, lower = 0, upper = 100, min.len = 1L,
    any.missing = FALSE, all.missing = FALSE)

  re::catf("[ecr3vis] plot_eaf_diff is under development and not yet finished.\n")

  #FIXME: add more sanity checks on df1 and df2

  if (!requireNamespace("eaf", quietly = TRUE))
    re::stopf("[plot_scatter3d] Package \"eaf\" needed for this function to work.")

  if (length(intervals) == 1) {
    intervals = eaf:::seq.intervals.labels(round(seq(0,1 , length.out = 1 + intervals), 4), digits = 1)
  }

  df1 = prepare_pf_for_visualization(df1, obj.cols, n.obj = 2L)
  df2 = prepare_pf_for_visualization(df2, obj.cols, n.obj = 2L)

  # Calculate attainment surfaces
  eaf1 = eaf::eafs(points = df1[, obj.cols], sets = df1$repl, groups = df1$algorithm, percentiles = percentiles)
  eaf2 = eaf::eafs(points = df2[, obj.cols], sets = df2$repl, groups = df2$algorithm, percentiles = percentiles)
  colnames(eaf1) = c("y1", "y2", "percentiles", "group")
  colnames(eaf2) = c("y1", "y2", "percentiles", "group")
  eaf1$group = eaf2$group = NULL
  eaf1$side = "left"
  eaf2$side = "right"
  eaf_both = rbind(eaf1, eaf2)

  # Compute difference in EAF
  # Need different sets
  df1$repl = as.integer(df1$repl)
  df2$repl = as.integer(df2$repl)
  df2$repl = df2$repl + max(df1$repl)
  data.combined = rbind(df1, df2)

  eafdiff = eaf:::compute.eafdiff(data.combined[, c(obj.cols, "repl"), drop = FALSE], intervals = length(intervals))
  diff_left = as.data.frame(eafdiff$left)
  diff_right = as.data.frame(eafdiff$right)

  # Combine diff data in data frame (long format) for ggplot
  colnames(diff_left) = c("x1", "x2", "diff")
  colnames(diff_right) = c("x1", "x2", "diff")
  diff_left$side = "left"
  diff_right$side = "right"
  diff_both = rbind(diff_left, diff_right)

  # Build the plot
  g = ggplot2::ggplot(data = diff_both)
  g = g + ggplot2::geom_point(mapping = ggplot2::aes_string(x = "x1", y = "x2", colour = "diff"), alpha = 0.1)
  g = g + ggplot2::facet_wrap(. ~ side, ncol = 2L)

  # Add attainment surface(s)
  eaf_both$percentiles = as.factor(eaf_both$percentiles)
  g = g + ggplot2::geom_step(data = eaf_both,
    mapping = ggplot2::aes_string(x = "y1", y = "y2", linetype = "percentiles"))
  g = g + ggplot2::theme_minimal()
  g = g + ggplot2::labs(
    x = "y1",
    y = "y2",
    linetype = "Attainment surface",
    colour = "Difference")
  #g = g + ggplot2::scale_color_grey()
  g = g + ggplot2::theme(legend.position = "bottom")
  return(g)
}
