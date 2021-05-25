#include <Rcpp.h>
#include <math.h>
#include <limits>

using namespace Rcpp;

/* Coverage / C-metric
 *
 * @param A [matrix]
 *   Matrix of points (column-wise).
 * @param np [numeric}]
 *   Nadir point (or approximation).
 * @param ip [numeric}]
 *   Ideal point (or approximation).
 * @return [numeric(1)] Coverage.
**/

// [[Rcpp::export]]
double cov_c(NumericMatrix A, NumericMatrix B) {
  unsigned int n = A.ncol();
  unsigned int m = A.nrow();

  double cov = 0;

  //FIXME: worst case running time is O(m * |A| * |B|) and this is tight
  for (unsigned int i = 0; i < B.ncol(); ++i) {
    bool isdom = false;;
    unsigned int j = 0;

    while (!isdom && (j < n)) {
      // check if B(_, i) is dominated by A(_, j)
      bool ajdombi = true;;
      unsigned int k = 0;
      unsigned int prec = 0;
      while (ajdombi && (k < m)) {
        if (A(k, j) > B(k, i)) {
          ajdombi = false;
        } else if (A(k, j) < B(k, i)) {
          prec += 1; // strictly better
        }
        ++k;
      }
      isdom = isdom | ((ajdombi) & (prec > 0));
      ++j;
    }
    if (isdom) {
      cov += 1;
      break;
    }
  }

  return cov / B.ncol();
}
