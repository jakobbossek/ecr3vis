#' @title
#' Calculate multi-objective performance indicators
#'
#' @description
#' The function expects a data frame with Pareto-front approximation sets given
#' in the columns passed by \code{obj.cols} and optional meta-columns \dQuote{problem}
#' \dQuote{algorithm} and \dQuote{repl}. Given a set of multi-objective performance
#' indicators, the function splits the data by the meta-columns and calculates the
#' indicator values for each approximation set.
#'
#' @template section_parallelization
#'
#' @param x [\code{data.frame}]\cr
#'   Input data frame. There must be at least the variables specified in \code{obj.cols}.
#' @template arg_obj_cols
#' @param unary [\code{list}]\cr
#'   Named list of indicators. The names must be strings that correspond to
#'   the function name of the indicator (e.g., \dQuote{gd} for \code{\link{gd}}).
#'   The value is an (possibly empty) named list of parameter values for the indicator
#'   (e.g. \code{list(p = 2)} to modify the \eqn{p} parameter of the generational
#'   distance indicator \code{\link{gd}}).
#' @param rsets [\code{list}]\cr
#'   Named list of reference sets in form of data frames. The names need to correspond
#'   to problem names in column \code{x$problem}. For all problems where no explicit
#'   reference set is given, the set is approximated as the non-dominated set of
#'   points of the union of all approximations for each problem.
#' @param format [\code{character(1)}]\cr
#'   If \dQuote{long}, the data is returned as a data frame in long format. I.e.,
#'   there is a column \dQuote{indicator} and another column \dQuote{value} for
#'   the corresponding indicator values. This format is the default and particulary
#'   helpful for visualization with \pkg{ggplot2}.
#'   In contrast, for format \dQuote{wide}, there is one column for each
#'   indicator. This format is less redundant and memory-intensive.
#' @return A data frame with columns \dQuote{problem},
#'   \dQuote{algorithm}, \dQuote{repl}, and columns with the respective
#'   indicator values (see argument \code{format} for details).
#'
#' @keywords optimize
#' @template family_multi_objective_performance_indicators
#' @export
#' @examples
#' # load sample data set
#' data(emoas_on_zdt)
#'
#' # get indicators in long format
#' inds = df_get_indicators(emoas_on_zdt, obj.cols = c("y1", "y2"),
#'   unary = list(
#'     hv = list(), # hv has no parameters
#'     rse = list(s = 0.5),
#'     ahd = list(p = 2)))
df_get_indicators = function(x, obj.cols, unary, rsets = list(), format = "long") {
  checkmate::assert_data_frame(x, min.cols = 3L, min.rows = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_subset(obj.cols, choices = colnames(x), empty.ok = FALSE)
  checkmate::assert_list(unary, types = "list")
  checkmate::assert_list(rsets, types = "data.frame")
  checkmate::assert_choice(format, choices = c("long", "wide"))

  # split by (problem, algorithm, repl)
  sep = "-----"
  xs = split(x, f = list(x$problem, x$algorithm, x$repl), drop = TRUE, sep = sep)

  inds.names = names(unary)
  prob.names = names(xs)

  problem.grid = expand.grid(problem = prob.names, indicator = inds.names, stringsAsFactors = FALSE)
  nps = df_get_nadir(x, obj.cols, as.df = FALSE)
  rsets = re::insert(df_get_reference_sets(x, obj.cols, as.df = FALSE), rsets)

  inds.unary = future.apply::future_sapply(re::df_rows_to_list(problem.grid), function(e) {
    meta = strsplit(e$problem, split = sep, fixed = TRUE)[[1L]]
    problem = meta[1L]
    algorithm = meta[2L]
    repl = as.integer(meta[3L])

    # input data, i.e., point set, reference set and nadir point
    ind.args = list(
      x = t(as.matrix(xs[[e$problem]][, obj.cols, drop = FALSE])),
      y = t(as.matrix(rsets[[problem]][, obj.cols, drop = FALSE])),
      r = nps[[problem]]
    ) # list

    # add arguments for indicator
    ind.args = re::insert(ind.args, unary[[e$indicator]])

    # calculate indicator value
    ind = try({do.call(e$indicator, ind.args)}, silent = TRUE)
    if (inherits(ind, "try-error"))
      return(NA_real_)
    return(ind)
  }, future.globals = FALSE, future.packages = "ecr3vis", future.seed = TRUE)

  problem.grid$value = inds.unary

  meta = re::df_explode(problem.grid, split.col = "problem", split = sep, names = c("problem", "algorithm", "repl"),
    types = c("cci"), keep = FALSE)

  if (format == "long")
    return(meta)

  # convert to wide format
  id.vars = setdiff(colnames(meta), c("indicator", "value"))
  reshape2::dcast(meta, stats::as.formula(paste0(re::collapse(id.vars, sep = " + "), "~ indicator")))
}
