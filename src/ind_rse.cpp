#include <Rcpp.h>
#include <math.h>
#include <limits>

using namespace Rcpp;

/* Riesz s-energy
 *
 * Calculation takes time \Theta(n^2)
 *
 * @param A [matrix]
 *   Matrix of points (column-wise).
 * @param s [numeric}]
 *   Uniformity parameter s >= 0.
 * @return [numeric(1)] Overall (Pareto) spred.
**/

// [[Rcpp::export]]
double rse_c(NumericMatrix A, double s) {
  unsigned int n = A.ncol();
  unsigned int m = A.nrow();

  double riesz = 0;
  for (unsigned int i = 0; i < n; ++i) {
    for (unsigned int j = i + 1; j < n; ++j) {
      double dist = 0;
      // calculate Euclidean distance
      for (unsigned int k = 0; k < m; ++k) {
        dist += (A(k, i) - A(k, j)) * (A(k, i) - A(k, j));
      }
      dist = sqrt(dist);
      if (s == 0) {
        dist = pow(dist, (-1) * s);
      } else {
        dist = (-1) * log(dist);
      }
      riesz += dist;
    }
  }

  return riesz;
}
