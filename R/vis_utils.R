# DATA TRANSFORMATION / PREPROCESSING
# ===

# Sanity checks and preprocessing for PF-visualization
#
# @param df [\code{data.frame()}]\cr
#  Data frame, i.e., Pareto-front approximation.
# @param obj.cols [\code{character}]\cr
#  Names of objective columns.
# @param n.obj [\code{integer(1) | NULL}]\cr
#  Number of objectives required.
#  Defaults to NULL.
# @return [\code{data.frame}] Modified data frame.
prepare_pf_for_visualization = function(df, obj.cols, n.obj = NULL) {
  # check if obj column exist
  if (!all(obj.cols %in% colnames(df)))
    re::stopf("obj.cols needs to contain valid column names.")

  if (!is.null(n.obj))
    if (length(obj.cols) != n.obj)
      re::stopf("Number of objectives needs to be %i, but is %i.", n.obj, length(obj.cols))

  # all objective need to be numeric
  df.obj = df[, obj.cols, drop = FALSE]
  obj.cols.numeric = sapply(df.obj, is.numeric)
  if (!all(obj.cols.numeric))
    re::stopf("Only numeric values allowed in obj.cols, but column(s) '%s' %s not numeric!",
      re::collapse(obj.cols[which(!obj.cols.numeric)], ifelse(sum(!obj.cols.numeric) > 1L, "are", "is")))

  # add meta data if missing
  if (is.null(df$algorithm))
    df$algorithm = "Algorithm"
  if (is.null(df$problem))
    df$problem = "Problem"
  if (is.null(df$repl))
    df$repl = as.factor(1L)

  df$repl = as.factor(df$repl)
  return(df)
}

# @title Convert a pf approximation into ggplot-friendly format
#
# @param df [\code{data.frame}]\cr
#  Pareto-front approximation with columns \code{obj.cols} and optionl further
#  columns.
# @param obj.cols [\code{character}]\cr
#  Names of objective columns.
# @param solutions.as.factor [\code{logical(1)}]\cr
#  If \code{TRUE}, solutions are numbered consecutively as they appear in
#  \code{df}.
#  Default is \code{FALSE}.
# @return [\code{data.frame}] \code{df} in long format with factor column
#  \dQuote{objective} and numeric column \dQuote{value}.
to_long_with_objective_column = function(df, obj.cols, solution.as.factor = FALSE) {
  df$nr = 1:nrow(df) # needed for grouping
  if (solution.as.factor)
    df$nr = factor(df$nr, ordered = TRUE)
  df = reshape2::melt(
    data = df,
    id.vars = setdiff(colnames(df), obj.cols),
    value.name = "value",
    variable.name = "objective")
  df$objective = factor(df$objective, levels = obj.cols, ordered = TRUE)
  return(df)
}
