#' @title
#' (Modified) (Inverted) Generational distance.
#'
#' @description
#' The generational distance (GD) measure the distance of a point set
#' \eqn{X = \{r_1, \ldots, r_{|X|}\}}, e.g., a Pareto-front approximation, to
#' a reference set \eqn{R = \{r_1, \ldots, r_{|R|}\}}. Then GD is defined as
#' \deqn{
#'  GD_p(A, R) = \frac{1}{|X|} \left(\sum_{i=1}^{|X|} d_i^p\right)^{1/p}
#' }
#' where \eqn{d_i} is the Euclidean distance of point \eqn{x_i \in X} to
#' its nearest neigbor point in \eqn{R}. The inverted generational distance works
#' the other way around, i.e.
#' \deqn{
#'  IGD_p(A, R) = \frac{1}{|R|} \left(\sum_{i=1}^{|R|} \hat{d}_i^p\right)^{1/p}
#' }
#' where \eqn{\hat{d}_i} is the respective nearest neighbor distance of \eqn{r_i}
#' to any point in \eqn{X}. Put differently, \eqn{IGD_p(A, R) = GD_p(R, A)}.
#' Functions \code{gd} and \code{igd} calcute these versions.
#'
#' Schütze et al. [2] proposed a slight modification:
#' \deqn{
#'  GD_p(A, R) = \left(\frac{1}{|X|} \sum_{i=1}^{|X|} d_i^p\right)^{1/p}
#' }
#' where the average is taken before the power operation. \eqn{IGD_p} is apdated
#' analogeously. This versions are calclated by \code{gd} and \code{igd} if argument
#' \code{modified} is set \code{TRUE}.
#'
#' Ishibushi et al. [3] proposed another modification which works on the
#' formulation by Schütze et al. (see above). They modified the distance
#' calculation:
#' \deqn{
#'  GD_p^{+}(A, R) = \left(\frac{1}{|X|} \sum_{i=1}^{|X|} d^{+^p}_i\right)^{1/p}
#' }
#' where \eqn{d_i^{+} = \max\{x_i, z_i\}}. This version can be calculated
#' with function \code{gdp} (the trailing p stands for \dQuote{plus}).
#'
#' Eventuelly, function \code{ahd} calculates the Average Hausdorff distance [2]
#' which combines GD and IGD and is defined as
#' \deqn{
#'  \delta_p(A, R) = \max\{GD_p(A, R), IGD_p(A, R)\}.
#' }
#'
#' @references
#' [1] David A. Van Veldhuizen and David A. Van Veldhuizen. Multiobjective
#' evolutionary algorithms: classifications, analyses, and new innovations.
#' Technical Report, Evolutionary Computation, 1999.
#'
#' [2] Schütze, O., Esquivel, X.,Lara,A. ,Coello, C.A.C.: Using the averaged
#' Hausdorff distance as a performance measure in evolutionary multiobjective
#' optimization. IEEE Transactions on Evolutionary Computation 16, 504–522 (2012).
#'
#' [3] Hisao Ishibuchi, Hiroyuki Masuda, Yuki Tanigaki, and Yusuke Nojima.
#' Modified distance calculation in generational distance and inverted generational
#' distance. In António Gaspar-Cunha, Carlos Henggeler Antunes, and Carlos
#' Coello Coello, editors, Evolutionary Multi-Criterion Optimization, 110–125.
#' Cham, 2015. Springer International Publishing.
#'
#'
#' @param x [\code{matrix}]\cr
#'   Point set in column major format.
#' @param y [\code{matrix}]\cr
#'   Reference point set in column major format.
#' @param p [\code{numeric(1)}]\cr
#'   Parameter \eqn{p} (see description).
#' @param modified [\code{logical(1)}]\cr
#'   Should the modified GD/IGD calculation by Schuetze et al. [2] be used?
#'   Default is \code{TRUE}.
#' @return [\code{numeric(1)}] Scalar indicator value.
#'
#' @keywords optimize
#' @family mootools
#' @rdname gd
#' @export
gd = function(x, y, p = 2, modified = TRUE) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(y, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_number(p, lower = 1, na.ok = FALSE)
  checkmate::assert_flag(modified)

  if (nrow(x) != nrow(y))
    re::stopf("[gd] Both point sets must have the same dimension.")

  .Call("_ecr3vis_gd_c", x, y, p, modified)
}

#' @rdname gd
#' @export
igd = function(x, y, p = 2, modified = TRUE) {
  gd(y, x, p, modified)
}

#' @rdname gd
#' @export
gdp = function(x, y, p = 2, modified = TRUE) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(y, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_number(p, lower = 1, na.ok = FALSE)

  if (nrow(x) != nrow(y))
    re::stopf("[gdp] Both point sets must have the same dimension.")

  .Call("_ecr3vis_gdp_c", x, y, p, modified)
}

#' @rdname gd
#' @export
igdp = function(x, y, p = 2, modified = TRUE) {
  gdp(y, x, p, modified)
}

#' @rdname gd
#' @export
ahd = function(x, y, p = 2, modified = TRUE) {
  gd = gd(x, y, p, modified)
  igd = igd(y, x, p, modified)
  max(gd, igd)
}
