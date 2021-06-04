#' @title
#' 2D scatter plot
#'
#' @description
#' Given a data frame with the results of (multiple) runs of (multiple)
#' different multi-objective optimization algorithms on (multiple) problem instances
#' the function generates \code{\link[ggplot2]{ggplot}} scatter-plots of the obtained
#' Pareto-front approximations.
#'
#' @references
#' [1] T. Tušar and B. Filipič, "Visualization of Pareto Front Approximations in
#' Evolutionary Multiobjective Optimization: A Critical Review and the Prosection
#' Method," in IEEE Transactions on Evolutionary Computation, vol. 19, no. 2,
#' pp. 225-245, April 2015, doi: 10.1109/TEVC.2014.2313407
#'
#' @template family_multi_objective_visualizations
#'
#' @template arg_df
#' @template arg_obj_cols
#' @param highlight.algos [\code{character(1)}]\cr
#'   Name of algorithm to highlight exclusively. Useful to highlight, e.g., the
#'   true Pareto-optimal front (if known) or some reference set.
#'   Default is \code{NULL}, i.e., unknown.
#' @param offset.highlighted [\code{numeric(1)}]\cr
#'   Numeric offset used to shift set (see \code{highlight.algos})
#'   which should be highlighted.
#'   Even though this produces objective vectors it
#'   may be used to make visible reference sets which otherwise would
#'   be hidden by overlap of multiple other approximation sets.
#' @param shape [\code{character(1)}]\cr
#'   Name of column which shall be used to define shape of points.
#'   Default is \dQuote{algorithm}.
#' @param colour [\code{character(1)}]\cr
#'   Name of column which shall be used to define colour of points.
#'   Default is \code{NULL}, i.e., coloring is deactivated.
#' @param size [\code{character(1)}]\cr
#'   Name of column which shall be used to define the size of points.
#'   Default is \code{NULL}, i.e., sizing is deactivated.
#'   Useful to visualize a third objective (bubble-chart; see argument \code{bubble}).
#' @param bubble [\code{logical(1)}]\cr
#'   Plot bubble-chart? I.e. is \code{colour} used to highlight another
#'   objective?
#'   Default is \code{FALSE}.
#' @param title [\code{character(1)}]\cr
#'   Plot title.
#' @param subtitle [\code{character(1)}]\cr
#'   Plot subtitle.
#' @param facet.type [\code{character(1)}]\cr
#'   Which faceting method to use? Pass \dQuote{wrap} for \code{\link[ggplot2]{facet_wrap}}
#'   or \dQuote{grid} for \code{\link[ggplot2]{facet_grid}}.
#'   Default is \dQuote{wrap}.
#' @param facet.args [\code{list}]\cr
#'   Named list of arguments passed down to \code{\link[ggplot2]{facet_wrap}} or
#'   \code{\link[ggplot2]{facet_grid}} respectively (depends on \code{facet.type}).
#'   E.g., \code{nrow} to change layout.
#'   Default is the empty list. In this case data is grouped by problem.
#' @template return_ggplot
#' @export
plot_scatter2d = function(
  df,
  obj.cols = c("y1", "y2"),
  shape = "algorithm",
  colour = NULL,
  size = NULL,
  bubble = FALSE,
  highlight.algos = NULL,
  offset.highlighted = 0,
  title = NULL, subtitle = NULL,
  facet.type = "wrap",
  facet.args = list()) {
  checkmate::assert_data_frame(df, min.rows = 2L, min.cols = 2L)
  checkmate::assert_character(obj.cols, min.len = 2L)
  checkmate::assert_flag(bubble)
  checkmate::assert_number(offset.highlighted, lower = 0, finite = TRUE)
  checkmate::assert_choice(facet.type, choices = c("wrap", "grid"))
  checkmate::assert_list(facet.args)

  df = prepare_pf_for_visualization(df, obj.cols, n.obj = 2L)

  checkmate::assert_choice(shape, choices = setdiff(colnames(df), obj.cols))
  checkmate::assert_choice(colour, choices = setdiff(colnames(df), obj.cols), null.ok = TRUE)

  # get algorithm names
  algos = unique(df$algorithm)
  probs = unique(df$problem)

  # get number of problems and algorithms
  n.algos = length(algos)
  n.probs = length(probs)

  if (!is.null(highlight.algos))
    checkmate::assert_choice(highlight.algos, choices = algos)

  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(subtitle, null.ok = TRUE)

  g = ggplot2::ggplot(mapping = ggplot2::aes_string(x = obj.cols[1L], y = obj.cols[2L]))

  data = df
  data.highlight = NULL

  if (!is.null(highlight.algos)) {
    data = df[df$algorithm != highlight.algos, , drop = FALSE]
    data.highlight = df[df$algorithm == highlight.algos, , drop = FALSE]
    data.highlight[, obj.cols] = data.highlight[, obj.cols] - offset.highlighted
  }

  if (!is.null(data.highlight)) {
    g = g + ggplot2::geom_step(
      data = data.highlight,
      alpha = 0.3)
    g = g + ggplot2::geom_line(
      data = data.highlight,
      alpha = 0.3)
    # g = g + ggplot2::geom_point(
    #   data = data.highlight,
    #   size = 3.5,
    #   shape = 1,
    #   alpha = 0.8,
    #   colour = "tomato")
  }
  g = g + ggplot2::geom_point(
    data = data,
    mapping = ggplot2::aes_string(shape = shape, colour = colour, size = size),
    alpha = 0.5)
  g = g + ggplot2::scale_shape_manual(values = 1:nlevels(as.factor(data[[shape]])))
  if (n.probs > 1L) {
    # how to group stuff
    group.by = if (facet.type == "wrap") formula( ~ problem) else formula(. ~ problem)
    default.facet.args = list(facets = group.by, scale = "free")
    facet.args = re::insert(default.facet.args, facet.args)
    if (facet.type == "wrap")
      g = g + do.call(ggplot2::facet_wrap, facet.args)
    else
      g = g + do.call(ggplot2::facet_grid, facet.args)
  }
  g = g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    shape = "Algorithm",
    colour = "Algorithm",
    size = "Algorithm"
  )
  g = g + ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )
  if (!is.null(colour)) {
    if (!bubble) {
      g = g + ggplot2::scale_color_brewer(palette = "Dark2")
    } else {
      if (requireNamespace("viridis", quietly = TRUE)) {
        g = g + viridis::scale_color_viridis()
      } else {
        re::catf("[plot_scatter2d] Package \"viridis\" not installed. Using default ggplot2 colors.")
      }
      g = g + ggplot2::labs(colour = sprintf("Obj. %s", colour))
    }
  }

  if (!is.null(size) & bubble)
    g = g + ggplot2::labs(size = sprintf("Obj. %s", size))

  g = g + ggplot2::theme_minimal()
  return(g)
}
