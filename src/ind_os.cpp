#include <Rcpp.h>
#include <math.h>
#include <limits>

using namespace Rcpp;

/* Overall (Pareto) spread (OS)
 *
 * @param A [matrix]
 *   Matrix of points (column-wise).
 * @param np [numeric}]
 *   Nadir point (or approximation).
 * @param ip [numeric}]
 *   Ideal point (or approximation).
 * @return [numeric(1)] Overall (Pareto) spred.
**/

// [[Rcpp::export]]
double os_c(NumericMatrix A, NumericVector np, NumericVector ip) {
  unsigned int n = A.ncol();
  unsigned int m = A.nrow();

  double os = 1;

  for (unsigned int i = 0; i < m; ++i) {
    double maxfi = A(i, 0);
    double minfi = A(i, 0);
    // FIXME: solve the min/max search with 3n/2 comparisons
    for (unsigned int j = 1; j < n; ++j) {
      if (A(i, j) > maxfi) {
        maxfi = A(i, j);
      }
      if (A(i, j) < minfi) {
        minfi = A(i, j);
      }
    }
    os *= (maxfi - minfi) / (np[i] - ip[i]);
  }

  return os;
}
