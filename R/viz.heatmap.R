#' @title Heatmap visualization
#'
#' @description Visualize a Pareto-front approximation by means of a heatmap.
#' Here, on the x-axis the objectives are given in the order provided by
#' \code{obj.cols}. Tthe y-axis shows the different solutions. The color of the
#' cells indicates the objective value of the respective (objective, solution)
#' pair. Works with any kind of objectives.
#'
#' @template arg_df
#' @template arg_obj_cols
#' @return [\code{\link[ggplot2]{ggplot}}] ggplot object.
#' @export
plot_heatmap = function(
  df,
  obj.cols = c("y1", "y2")) {

  assertDataFrame(df, min.rows = 2L, min.cols = 2L)
  assertCharacter(obj.cols, min.len = 2L)

  df = to_long_with_objective_column(df, obj.cols, solution.as.factor = TRUE)

  g = ggplot(df, mapping = aes_string(x = "objective", y = "nr", fill = "value"))
  g = g + geom_tile()
  g = g + scale_fill_brewer(palette = "Dark2")
  g = g + theme_minimal()
  g = g + labs(x = "Objective", y = "Solution")
  return(g)
}
