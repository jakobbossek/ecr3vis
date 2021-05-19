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
  if (is.null(df$prob))
    df$prob = "Problem"
  if (is.null(df$repl))
    df$repl = as.factor(1L)

  df$repl = as.factor(df$repl)
  return(df)
}
