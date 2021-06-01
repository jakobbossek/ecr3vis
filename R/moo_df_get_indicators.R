#' @title
#' Calculate multi-objective performance indicators
#'
#' @description
#' The function expects a data frame with Pareto -front approximation sets given
#' in the columns passed by \code{obj.cols} and optional meta-columns \dQuote{problem}
#' \dQuote{algorithm} and \dQuote{repl}. Given a set of multi-objective performance
#' indicators, the function splits the data by the meta-columns and calculates the
#' indicator values for each approximation set.
#'
#' @param x [\code{data.frame}]\cr
#'   Input data frame. Requires at least the column names given by \code{obj.cols}.
#' @template arg_obj_cols
#' @param unary [\code{list}]\cr
#'   Named list of indicators. The names must be strings which correspond to
#'   the function name of the indicator (e.g., \dQuote{gd} for \code{\link{gd}}).
#'   The value is an (possibly empty) named list of parameter values for the indicator
#'   (e.g. \code{list(p = 2)} to modify the \eqn{p} parameter of the generational
#'   distance indicator \code{\link{gd}}).
#' @param format [\code{character(1)}]\cr
#'   If \dQuote{long}, the data is returned as a data frame in long format. I.e.,
#'   there is a column \dQuote{indicator} and another column \dQuote{value} for
#'   the corresponding indicator values. This format is the default and particulary
#'   helpful for visualization with \pkg{ggplot2}.
#'   In contrast, for format \dQuote{wide}, there is one column for each
#'   indicator. This format is less redundant and memory-intensive.
#' @return [\code{data.frame}] A data frame with columns \dQuote{problem},
#'   \dQuote{algorithm}, \dQuote{repl} and columns with the respective
#'   indicator values (see argument \code{format} for details).
#'
#' @keywords optimize
#' @family mootools
#' @family multi-objective performance indicators
#' @export
df_get_indicators = function(x, obj.cols, unary, format = "long") {
  #FIXME: add option to pass (named) lists of reference points and reference sets
  #FIXME: parallelize via future
  checkmate::assert_data_frame(x, min.cols = 3L, min.rows = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_choice(obj.cols, choices = colnames(x))
  checkmate::assert_list(unary, types = "list")
  checkmate::assert_choice(format, choices = c("long", "wide"))

  # split by (prob, algorithm, repl)
  #FIXME: make this a parameter?
  sep = "-----"
  xs = split(x, f = list(x$prob, x$algorithm, x$repl), drop = TRUE, sep = sep)

  inds.names = names(unary)
  prob.names = names(xs)

  problem.grid = expand.grid(problem = prob.names, indicator = inds.names, stringsAsFactors = FALSE)
  nps = df_get_nadir(x, obj.cols, as.df = FALSE)
  rsets = df_get_reference_sets(x, obj.cols, as.df = FALSE)

  #FIXME: parallize via future
  inds.unary = sapply(re::df_rows_to_list(problem.grid), function(e) {
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
  })

  #FIXME: ugly as hell! Why is re::explode defined for single string only and
  # why does re::df_split_col has a different interface?!?
  #FIXME: reshape::dcast to convert into wide format if option is set.
  problem.grid$value = inds.unary
  meta = re::rbindlapply(problem.grid$problem, re::explode, split = sep, names = c("problem", "algorithm", "repl"), types = c("cci"))
  cbind(meta, problem.grid[, -1L, drop = FALSE])
}
