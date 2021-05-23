#' @title \eqn{\varepsilon}-indicator
#'
#' @description ...
#'
#' @param x [\code{matrix}]\cr
#'   Point set in column major format.
#' @param y [\code{matrix}]\cr
#'   Reference point set in column major format.
#' @return [\code{numeric(1)}] Scalar indicator value.
#'
#' @keywords optimize
#' @family mootools
#' @rdname gd
#' @export
eps = function(x, y) {
    # sanity checks
    checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
    checkmate::assert_matrix(y, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
    if (nrow(x) != nrow(y))
      re::stopf("[eps] Point sets x and y must have the same number of rows.")

    return(.Call("eps_c", x, y))
  }
