#' @title Parallel coordinate plot
#'
#' @description Visualizes a Pareto-front approximation with parallel coordinate
#' plots. The x-axis shows the objectives as an ordered factor where the order
#' is determined by the \code{obj.cols} argument. On the y-axis objective values are
#' shown colored by solution as lines. Suitable for any number of objectives.
#'
#' [1] T. Tušar and B. Filipič, "Visualization of Pareto Front Approximations in
#' Evolutionary Multiobjective Optimization: A Critical Review and the Prosection
#' Method," in IEEE Transactions on Evolutionary Computation, vol. 19, no. 2,
#' pp. 225-245, April 2015, doi: 10.1109/TEVC.2014.2313407
#'
#' @template arg_df
#' @template arg_obj_cols
#' @return [\code{\link[ggplot2]{ggplot}}] ggplot object.
#' @family visualization
#' @export
plot_pcp = function(
  df,
  obj.cols = c("y1", "y2")) {

  assertDataFrame(df, min.rows = 2L, min.cols = 2L)
  assertCharacter(obj.cols, min.len = 2L)

  df = prepare_pf_for_visualization(df, obj.cols)
  df = to_long_with_objective_column(df, obj.cols)

  g = ggplot(df, mapping = aes_string(x = "objective", y = "value", group = "nr"))
  g = g + geom_path()
  g = g + geom_point()
  g = g + labs(x = "Objective", y = "Value")
  g = g + theme_minimal()
  return(g)
}
