#' @title
#' Radar/spider plot
#'
#' @description
#' A \emph{radar plot} or \emph{spider plot} is an elegant method to visualize
#' the quality of solutions with more than two objectives. Essentially it
#' shows multiple numeric values starting from the same origin in a circular
#' two-dimensional representation.
#'
#' Users who are into gaming might know the kind of visualization from games like
#' Pro Evolution Soccer, NHL or most likely any kind of sport games. Here, the
#' individual strength and weaknesses of players (speed, tequniqual skills,
#' fitness etc.) are often visualized by means of radar plots.
#'
#' The drawback of this method is that it is difficult to compare many solutions
#' or - even worse - many solutions from different algorithms to get the big picture.
#'
#' @references
#' [1] T. Tušar and B. Filipič, Visualization of Pareto Front Approximations in
#' Evolutionary Multiobjective Optimization: A Critical Review and the Prosection
#' Method, in IEEE Transactions on Evolutionary Computation, vol. 19, no. 2,
#' pp. 225-245, April 2015, doi: 10.1109/TEVC.2014.2313407.
#'
#' @template family_multi_objective_visualizations
#'
#' @template arg_df
#' @template arg_obj_cols
#' @template return_ggplot
#' @export
#' @examples
#' data(mtcars)
#'
#' # scale to [0, 1]
#' tbl = as.data.frame(scale(mtcars[1:6, ], center = FALSE, scale = TRUE))
#' tbl$car = rownames(tbl)
#'
#' \dontrun{
#' # 6 cars in one plot
#' g = plot_radar(tbl, c("mpg", "cyl", "hp"))
#' print(g)
#'
#' # split by car (-> 6 plots)
#' print(g + facet_wrap(car ~ .))
#' }
plot_radar = function(
  df,
  obj.cols = c("y1", "y2", "y3")) {

  # Taken from https://stackoverflow.com/questions/28898143/closing-the-lines-in-a-ggplot2-radar-spider-chart
  coord_radar = function(theta = "x", start = 0, direction = 1) {
    theta = match.arg(theta, c("x", "y"))
    r = if (theta == "x") "y" else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
      direction = sign(direction),
      is_linear = function(coord) TRUE)
  }

  df = to_long_with_objective_column(df, obj.cols, solution.as.factor = TRUE)

  # add another line of first objective to "close the lines"
  df1 = df[df$objective == obj.cols[1], ]
  df = rbind(df, df1)

  g = ggplot2::ggplot(data = df, mapping = ggplot2::aes_string(x  = "objective", y = "value"))
  g = g + ggplot2::geom_polygon(ggplot2::aes_string(group = "nr", fill = "nr"), alpha = 0.3)

  # artificial axis
  #g = g + geom_vline(xintercept = 100, color = "gray")
  g = g + ggplot2::geom_path(ggplot2::aes_string(group = "nr", color = "nr", linetype = "nr"), alpha = 0.3)
  g = g + ggplot2::geom_point(ggplot2::aes_string(group = "nr", color = "nr"))
  g = g + ggplot2::labs(x = "", y = "", fill = "Solution", color = "Solution", linetype = "Solution")
  g = g + ggplot2::ylim(c(0, max(df$value)))
  g = g + coord_radar() # see local function above
  g = g + ggplot2::scale_color_brewer(palette = "Dark2")
  g = g + ggplot2::scale_fill_brewer(palette = "Dark2")
  g = g + ggplot2::theme_minimal()
  return(g)
}
