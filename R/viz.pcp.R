#' @title Parallel coordinate plot
#'
#' @description Visualizes a Pareto-front approximation with parallel coordinate
#' plots. The x-axis shows the objectives as an ordered factor where the order
#' is determined by the \code{obj.cols} argument. On the y-axis objective values are
#' shown colored by solution as lines. Suitable for any number of objectives.
#'
#' @template arg_df
#' @template arg_obj_cols
#' @return [\code{\link[ggplot2]{ggplot}}] ggplot object.
#' @export
plot_pcp = function(df,
  obj.cols = c("y1", "y2")) {

  assertDataFrame(df, min.rows = 2L, min.cols = 2L)
  assertCharacter(obj.cols, min.len = 2L)

  df = to_long_with_objective_column(df, obj.cols)

  g = ggplot(df, mapping = aes_string(x = "objective", y = "value", group = "nr"))
  g = g + geom_path()
  g = g + geom_point()
  g = g + labs(x = "Objective", y = "Value")
  return(g)
}
