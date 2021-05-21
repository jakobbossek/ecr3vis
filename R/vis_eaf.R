#' @title Empirical attainment function (EAF)
#'
#' @description ...
#'
#' @references
#' [1] V. Grunert da Fonseca and C. M. Fonseca, “The attainment-function approach
#' to stochastic multiobjective optimizer assessment and comparison,” in
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
#' @template arg_df
#' @template arg_obj_cols
#' @param percentiles [\code{numeric}]\cr
#'   Percentiles of the EAF that will be plotted as attainment surfaces.
#' @return [\code{\link[ggplot2]{ggplot}}] ggplot object.
#' @family visualization
#' @export
plot_eaf = function(
  df,
  obj.cols,
  percentiles = c(0, 50, 75, 100)) {
  checkmate::assert_numeric(percentiles, lower = 0, upper = 100, min.len = 1L,
    any.missing = FALSE, all.missing = FALSE)

  df = prepare_pf_for_visualization(df, obj.cols, n.obj = 2L)

  #FIXME: group by (algorithm, prob)
  dfeaf = eaf::eafs(points = df[, obj.cols], sets = df$repl, groups = df$prob, percentiles = percentiles)
  colnames(dfeaf) = c("y1", "y2", "percentiles", "group")

  n.groups = re::nunique(dfeaf$group)
  dfeaf$percentiles = factor(dfeaf$percentiles, levels = percentiles, ordered = TRUE)

  g = ggplot2::ggplot(dfeaf, ggplot2::aes_string(x = "y1", y = "y2",
    linetype = "percentiles", colour = "percentiles"))
  g = g + ggplot2::geom_step()
  g = g + ggplot2::theme_minimal()
  g = g + ggplot2::labs(
    x = "y1",
    y = "y2",
    linetype = "Att. surface",
    colour = "Att. surface")
  g = g + ggplot2::scale_color_grey()
  g = g + ggplot2::theme(legend.position = "bottom")
  if (n.groups > 1L)
    g = g + ggplot2::facet_wrap(. ~ group, scales = "free")
  return(g)
}
