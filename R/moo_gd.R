#' @title
#' (Inverted) Generational Distance and Average Hausdorff Distance
#'
#' @description
#' These functions expect as mandatory arguments a point set \eqn{X = \{x_1, \ldots, x_{|X|}\}}
#' (parameter \code{x}) and  and a set of reference points
#' \eqn{Y = \{y_1, \ldots, y_{|Y|}\}} (parameter \code{y}). For details on the
#' optional argument \code{p} and \code{modified} see the section on details.
#' \itemize{
#'   \item \code{gd} calculates the Generational Distance (GD).
#'   \item \code{igd} calculates the Inverse Generational Distance (IGD).
#'   \item \code{gdp/igdp} compute the (I)GD+ indicator [3].
#'   \item \code{ahd} calculates the Average Hausdorff Distance.
#' }
#'
#' @details
#' The Generational Distance (GD) measures the distance of a point set
#' \eqn{X = \{r_1, \ldots, r_{|X|}\}}, e.g., a Pareto-front approximation, to
#' a reference set \eqn{R = \{r_1, \ldots, r_{|R|}\}}. Then GD is defined as
#' \deqn{
#'  GD_p(A, R) = \frac{1}{|X|} \left(\sum_{i=1}^{|X|} d_i^p\right)^{1/p}
#' }
#' where \eqn{d_i} is the Euclidean distance of point \eqn{x_i \in X} to
#' its nearest neigbor point in \eqn{R}. The Inverted Generational Distance works
#' the other way around, i.e.,
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
#' analogeously. This versions are calclated by \code{gd} and \code{igd} if the argument
#' \code{modified} is set to \code{TRUE}.
#'
#' Ishibushi et al. [3] proposed another modification which works on the
#' formulation by Schütze et al. (see above). They modified the distance
#' calculation:
#' \deqn{
#'  GD_p^{+}(A, R) = \left(\frac{1}{|X|} \sum_{i=1}^{|X|} d^{+^p}_i\right)^{1/p}
#' }
#' where \eqn{d_i^{+} = \max\{x_i, z_i\}}. This version can be calculated
#' with the function \code{gdp} (the trailing \emph{p} stands for \dQuote{plus}).
#'
#' Eventuelly, the function \code{ahd} calculates the Average Hausdorff Distance [2]
#' which combines GD and IGD and is defined as
#' \deqn{
#'  \Delta_p(A, R) = \max\{GD_p(A, R), IGD_p(A, R)\}.
#' }
#' By default, \code{ahd} uses the modified versions of \eqn{GD} and \eqn{IGD}
#' respectively (see argument \code{modified}).
#'
#' IGDX [4] is a meaasure for decision space diversity. This is simply IGD;
#' however, the input consists of the non-dominated solutions in decision space
#' rather in objective space. Naturally, all implemented functions can be used as
#' an \dQuote{*X} version.
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
#' [4] O. Schütze, M. Vasile, and C. A. C. Coello, Computing the Set of
#' Epsilon-Efficient Solutions in Multiobjective Space Mission Design,
#  JACIC, vol. 8, no. 3, pp. 53–70, 2011.
#'
#' @keywords optimize
#' @template family_multi_objective_performance_indicators
#'
#' @param x [\code{matrix}]\cr
#'   Numeric matrix of points (each colum contains one point).
#' @param y [\code{matrix}]\cr
#'   The reference set as a numeric matrix of points (each colum contains one point).
#' @param p [\code{numeric(1)}]\cr
#'   Parameter \eqn{p} (see description).
#' @param modified [\code{logical(1)}]\cr
#'   Should the modified GD/IGD calculation by Schuetze et al. [2] be used?
#'   Default is \code{TRUE}.
#' @template arg_dots_not_used
#' @return Single numeric indicator value.
#'
#' @rdname gd
#' @export
gd = function(x, y, p = 2, modified = TRUE, ...) {
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
igd = function(x, y, p = 2, modified = TRUE, ...) {
  gd(y, x, p, modified)
}

#' @rdname gd
#' @export
gdp = function(x, y, p = 2, modified = TRUE, ...) {
  checkmate::assert_matrix(x, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_matrix(y, mode = "numeric", min.rows = 2L, min.cols = 1L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assert_number(p, lower = 1, na.ok = FALSE)

  if (nrow(x) != nrow(y))
    re::stopf("[gdp] Both point sets must have the same dimension.")

  .Call("_ecr3vis_gdp_c", x, y, p, modified)
}

#' @rdname gd
#' @export
igdp = function(x, y, p = 2, modified = TRUE, ...) {
  gdp(y, x, p, modified)
}

#' @rdname gd
#' @export
ahd = function(x, y, p = 2, modified = TRUE, ...) {
  gd = gd(x, y, p, modified)
  igd = igd(y, x, p, modified)
  max(gd, igd)
}
