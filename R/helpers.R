# DATA TRANSFORMATION
# ===

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
