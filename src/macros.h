#ifndef FOO_MACROS_H
#define FOO_MACROS_H

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


#define UNPACK_REAL_VECTOR(S, D, N) \
    double *D = REAL(S); \
    const R_len_t N = length(S);

#define UNPACK_REAL_MATRIX(S, D, N, K) \
    double *D = REAL(S); \
    const R_len_t N = nrows(S); \
    const R_len_t K = ncols(S);

#define UNPACK_INT_MATRIX(S, D, N, K) \
    int *D = INTEGER(S); \
    const R_len_t N = nrows(S); \
    const R_len_t K = ncols(S);

#define UNPACK_REAL_MATRIX_2(S, D, N) \
    double *D = REAL(S); \
    const R_len_t N = nrows(S); 

#endif
