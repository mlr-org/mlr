#include "macros.h"
#include "Rmath.h"

SEXP c_smote(SEXP s_x, SEXP s_isnum, SEXP s_nn, SEXP s_res) {
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  int *isnum = LOGICAL(s_isnum);
  UNPACK_INT_MATRIX(s_nn, nn, nrow_nn, ncol_nn);
  UNPACK_REAL_MATRIX_2(s_res, res, nrow_res);
  double lambda;
  unsigned int j_sel;
  unsigned int j_nn;
  unsigned int lev;

  GetRNGstate();
  for (R_len_t i = 0; i < nrow_res; i++) {
    /* select a random minority obs and random neighbor */
    j_sel = runif(0, nrow_x);
    j_nn = runif(0, ncol_nn);
    j_nn = nn[j_sel + j_nn * nrow_nn] - 1;
    lambda = unif_rand();
    for (R_len_t col = 0; col < ncol_x; col++) {
      if (isnum[col]) {
        /* do convex combination for numerics */
        res[i + col * nrow_res] = lambda * x[j_sel + col * nrow_x] +  (1 - lambda) * x[j_nn + col * nrow_x];
      } else {
        /* for factors we sample a level from x1 or x2 */
        if (lambda < 0.5)
          lev = x[j_sel + col * nrow_x];
        else
          lev = x[j_nn + col * nrow_x];
        res[i + col * nrow_res] = lev;
      }
    }
  }
  PutRNGstate();
  return s_res;
}


