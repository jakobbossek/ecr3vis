df_get_indicators = function(x, obj.cols, unary, binary = NULL, format = "long") {
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
  problem.grid$value = inds.unary
  meta = re::rbindlapply(problem.grid$problem, re::explode, split = sep, names = c("problem", "algorithm", "repl"), types = c("cci"))
  cbind(meta, problem.grid[, -1L, drop = FALSE])
}
