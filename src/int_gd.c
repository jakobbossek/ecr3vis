#include <R.h>
#include <Rinternals.h>
#include <float.h>
#include <math.h>
#include <limits.h>
#include "macros.h"
#include "hv.h"

/* Interface to hypervolume algorithm by Fonseca et al.
 *
 * @param rA [matrix]
 *   Matrix of points (column-wise).
 * @param rB [numeric}]
 *   Reference point set.
 * @param rp
 * @return [numeric(1)] Generational distance.
**/
SEXP gd_c(SEXP rA, SEXP rB, SEXP rp) {
  // unpack R data
  EXTRACT_NUMERIC_MATRIX(rA, A, d_A, n_A);
  EXTRACT_NUMERIC_MATRIX(rB, B, d_B, n_B);
  EXTRACT_REAL(rp, p);

  // allocate memory for Hypervolume value
  SEXP out = ALLOC_REAL_VECTOR(1);
  double *gd = REAL(out);

  for (unsigned int i = 0; i < n_A; ++i) {
    double d_min = DBL_MAX;
    for (unsigned int j = 0; j < n_B; ++j) {
      double d = 0.0;
      for (unsigned int k = 0; k < d_A; ++k) {
        double m = (A[k, i] - B[k, j]);
        d += m * m;
      }
      d = sqrt(d);
      if (d < d_min) {
        d_min = d;
      }
    }
    gd[0] += pow(d_min, p);
  }
  //FIXME: there is also a version with pow(gd / n, 1.0 / p)
  gd[0] = pow(gd[0], 1.0 / p) / n_A;

  // free memory and return
  UNPROTECT(1);
  return out;
}

// GD+ as proposed by Ishibushi et al. in "Hisao Ishibuchi, Hiroyuki Masuda, Yuki Tanigaki, a
// nd Yusuke Nojima.
// Modified distance calculation in generational distance and inverted generational
// distance. In António Gaspar-Cunha, Carlos Henggeler Antunes, and Carlos
// Coello Coello, editors, Evolutionary Multi-Criterion Optimization, 110–125.
// Cham, 2015. Springer International Publishing."
SEXP gdp_c(SEXP rA, SEXP rB, SEXP rp) {
  // unpack R data
  EXTRACT_NUMERIC_MATRIX(rA, A, d_A, n_A);
  EXTRACT_NUMERIC_MATRIX(rB, B, d_B, n_B);
  EXTRACT_REAL(rp, p);

  // allocate memory for Hypervolume value
  SEXP out = ALLOC_REAL_VECTOR(1);
  double *gd = REAL(out);

  for (unsigned int i = 0; i < n_A; ++i) {
    double dplus_min = DBL_MAX;
    for (unsigned int j = 0; j < n_B; ++j) {
      double d = 0.0;
      for (unsigned int k = 0; k < d_A; ++k) {
        // here, the modified distance calculation takes place
        double m = (A[k, i] - B[k, j]) ? (A[k, i] - B[k, j]) : (0);
        d += m * m;
      }
      d = sqrt(d);
      if (d < d) {
        dplus_min = d;
      }
    }
    gd[0] += pow(dplus_min, p);
  }
  //FIXME: there is also a version with pow(gd / n, 1.0 / p)
  gd[0] = pow(gd[0] / n_A, 1.0 / p);

  // free memory and return
  UNPROTECT(1);
  return out;
}
