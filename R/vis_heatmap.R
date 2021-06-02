#' @title
#' Heatmap
#'
#' @description
#' Visualize a Pareto-front approximation set by means of a heatmap.
#' Here, on the \eqn{x}-axis the objectives are given in the order provided by
#' \code{obj.cols}. The \eqn{y}-axis shows the different solutions. The color of the
#' cells indicates the objective value of the respective (objective, solution)-pair.
#' Works with any kind of objectives.
#'
#' @references
#' [1] T. Tušar and B. Filipič, Visualization of Pareto Front Approximations in
#' Evolutionary Multiobjective Optimization: A Critical Review and the Prosection
#' Method, in IEEE Transactions on Evolutionary Computation, vol. 19, no. 2,
#' pp. 225-245, April 2015, doi: 10.1109/TEVC.2014.2313407.
#'
#' @template arg_df
#' @template arg_obj_cols
#' @return [\code{\link[ggplot2]{ggplot}}] ggplot object.
#' @template family_multi_objective_visualizations
#' @export
plot_heatmap = function(
  df,
  obj.cols = c("y1", "y2")) {

  checkmate::assert_data_frame(df, min.rows = 2L, min.cols = 2L)
  checkmate::assert_character(obj.cols, min.len = 2L)

 # df = prepare_pf_for_visualization(df, obj.cols)
  df = to_long_with_objective_column(df, obj.cols, solution.as.factor = TRUE)

  g = ggplot2::ggplot(df, mapping = ggplot2::aes_string(x = "objective", y = "nr", fill = "value"))
  g = g + ggplot2::geom_tile()
  g = g + viridis::scale_fill_viridis(discrete = FALSE)
  g = g + ggplot2::labs(x = "Objective", y = "Solution")
  g = g + ggplot2::theme_minimal()
  return(g)
}
