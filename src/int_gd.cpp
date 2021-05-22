#include <Rcpp.h>
#include <math.h>
#include <limits>
//#include <algorithm>

using namespace Rcpp;

/* Generational distance (GD)
 *
 * @param A [matrix]
 *   Matrix of points (column-wise).
 * @param B [numeric}]
 *   Reference point set.
 * @param p [double]
 *   Parameter p.
 * @param modified [bool]
 *   Calculate modified version.
 * @return [numeric(1)] Generational distance.
**/

// [[Rcpp::export]]
double gd_c(NumericMatrix A, NumericMatrix B, double p, bool modified) {
  unsigned int n = A.ncol();
  double gd = 0;

  for (unsigned int i = 0; i < n; ++i) {
    double d_min = std::numeric_limits<double>::max();;
    for (unsigned int j = 0; j < B.ncol(); ++j) {
      double d = 0.0;
      for (unsigned int k = 0; k < A.nrow(); ++k) {
        double m = A(k, i) - B(k, j);
        d += m * m;
      }
      d = sqrt(d);
      if (d < d_min) {
        d_min = d;
      }
    }
    gd += pow(d_min, p);
  }
  if (modified) {
    gd = pow(gd / n, 1.0 / p);
  } else {
    gd = pow(gd, 1.0 / p) / n;
  }

  return gd;
}

/* GD+ as proposed by Ishibushi et al. in "Hisao Ishibuchi, Hiroyuki Masuda, Yuki Tanigaki,
 * and Yusuke Nojima.
 * Modified distance calculation in generational distance and inverted generational
 * distance. In António Gaspar-Cunha, Carlos Henggeler Antunes, and Carlos
 * Coello Coello, editors, Evolutionary Multi-Criterion Optimization, 110–125.
 * Cham, 2015. Springer International Publishing."
 */

// [[Rcpp::export]]
double gdp_c(NumericMatrix A, NumericMatrix B, double p, bool modified) {
  unsigned int n = A.ncol();
  double gd = 0;

  for (unsigned int i = 0; i < n; ++i) {
    double dplus_min = std::numeric_limits<double>::max();;
    for (unsigned int j = 0; j < B.ncol(); ++j) {
      double d = 0.0;
      for (unsigned int k = 0; k < A.nrow(); ++k) {
        // here, the modified distance calculation takes place
        double m = A(k, i) - B(k, j);
        if (m < 0) {
          m = 0;
        }
        d += m * m;
      }
      d = sqrt(d);
      if (d < dplus_min) {
        dplus_min = d;
      }
    }
    gd += pow(dplus_min, p);
  }
  if (modified) {
    gd = pow(gd / n, 1.0 / p);
  } else {
    gd = pow(gd, 1.0 / p) / n;
  }

  return gd;
}
