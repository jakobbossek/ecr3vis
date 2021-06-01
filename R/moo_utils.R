df_get_nadir = function(x, obj.cols, as.df = FALSE, ...) {
  if (is.null(x$problem))
    x$problem = 1
  if (is.null(x$algorithm))
    x$algorithm = 1
  x$repl = NULL

  idx.meta = !(colnames(x) %in% obj.cols)
  xs = split(x, f = list(x$problem), drop = TRUE)
  nps = lapply(xs, function(e) {
    get_nadir(t(as.matrix(e[, obj.cols, drop = FALSE])))
  })
  if (!as.df)
    return(nps)
  do.call(rbind, nps)
}

df_get_reference_sets = function(x, obj.cols, as.df = FALSE, ...) {
  if (is.null(x$problem))
    x$problem = 1
  if (is.null(x$algorithm))
    x$algorithm = 1
  #x$repl = NULL

  idx.meta = !(colnames(x) %in% obj.cols)
  xs = split(x, f = list(x$problem), drop = TRUE)
  lapply(xs, function(e) {
    res = e[1L, idx.meta, drop = FALSE]
    #FIXME: add which_nondominated.data.frame function to encapsulate this
    point.set = t(as.matrix(e[, obj.cols, drop = FALSE]))
    point.set = point.set[, which_nondominated(point.set), drop = TRUE]
    point.set = as.data.frame(t(point.set))
    colnames(point.set) = obj.cols
    return(point.set)
  })
}

#FIXME: df_get_nadir and df_get_reference_sets share redundant steps
# df_get = function(x, f, obj.cols, ...) {

# }
