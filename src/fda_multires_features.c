#include "macros.h"
#include "Rmath.h"
#include "math.h"

// extracts basic features from a curve segment
//   currently: only mean of curve
// the input is matrix x of dim (nrow_x, ncol_x)
// the segment starts in x from cell (x_read_row, x_read_col) and is x_read_len entries long
// the output is written to matrix resmat of dim (nrow_resmat, ncol_resmat)
// the writing starts from resmat cell (res_write_row, res_write_col) and is #features entries long
void c_get_segment_features(double* x, unsigned int nrow_x, unsigned int ncol_x,
  unsigned int x_read_row, unsigned int x_read_col, unsigned int x_read_len,
  double* resmat, unsigned int nrow_resmat, unsigned int ncol_resmat,
  unsigned int res_write_row, unsigned int res_write_col) {

  double mu = 0.0;
  for (R_len_t i = 0; i < x_read_len; i++) { // loop thru curve segment
    /* Rprintf("i = %i val=%g\n", i, x[x_read_row + nrow_x * (x_read_col + i)]); */
    mu += x[x_read_row + nrow_x * (x_read_col + i)];
  }
  mu = mu / x_read_len;
  /* Rprintf("write to (%i, %i); %i\n", res_write_row, res_write_col, res_write_row + nrow_resmat * res_write_col); */
  resmat[res_write_row + nrow_resmat * res_write_col] = mu; // write to resmat
}

// extracts multiresolution features from complete curve
//   we shift a window over the curve (by adding an offset) and when done half its size,
//   basic features are computed in every window
// the input is matrix x of dim (nrow_x, ncol_x)
// the curve starts in x from cell (x_read_row, x_read_col) and is x_read_len entries long
// the output is written to matrix resmat of dim (nrow_resmat, ncol_resmat)
// the writing starts from resmat cell (res_write_row, res_write_col),
// how many entries are written will be stored in rcounter
// reslev: maximal resolution "level" = so many times (minus 1) the initial window size is halfed
// shift [0,1]: offset for window shifting is shift*window_size
void c_get_curve_features(double* x, unsigned int nrow_x, unsigned int ncol_x,
  unsigned int x_read_row, unsigned int x_read_col, unsigned int x_read_len,
  double* resmat, unsigned int nrow_resmat, unsigned int ncol_resmat,
  unsigned int res_write_row, unsigned int res_write_col, unsigned int* rcounter,
  unsigned int reslev, double shift) {

  unsigned int sstart, ssize, soffset;
  unsigned int nfeats = 1;
  *rcounter = 0; // stores how many features where computed / output cells written
  ssize = x_read_len; // initial window is complete curve
  for (R_len_t rl = 1; rl <= reslev; rl++) { // iterate over resolution levels from 1 to reslev
    sstart = x_read_col; // start at beggining of curve
    soffset = ceil(shift * ssize); // offset we shift window to right
    Rprintf("reslev=%i, ssize=%i; soffset=%i\n", rl, ssize, soffset);
    while (sstart + ssize <= x_read_col + x_read_len) {
      Rprintf("start=%i; len=%i; offset=%i\n", sstart, ssize, soffset);
      c_get_segment_features(x, nrow_x, ncol_x, x_read_row, sstart, ssize, // get features from segment: (sstart, ssize)
        resmat, nrow_resmat, ncol_resmat, res_write_row, res_write_col); // and write feats to resmat
      sstart += soffset; // shift window to right by offset
      res_write_col += nfeats; // inc resmat index by nr of computed / written features
      *rcounter += nfeats; // count nr of written features in resmat
    }
    ssize = ceil(ssize / 2.0); // half window size
    if (ssize < 1) // if window too small, we are also done
      break;
  }
}

// extracts multiresolution features from multiple curves in multiple observation (rows)
// see docs above for most args
// and R function extractMultiResFeatures
void c_get_multires_curve_features(SEXP s_x, SEXP s_curve_lens, SEXP s_resmat, SEXP s_reslev, SEXP s_shift) {
  // unpack our args
  UNPACK_REAL_MATRIX(s_x, x, nrow_x, ncol_x);
  UNPACK_REAL_VECTOR(s_curve_lens, curve_lens, n_curves);
  UNPACK_REAL_MATRIX(s_resmat, resmat, nrow_resmat, ncol_resmat);
  unsigned int reslev = asInteger(s_reslev);
  double shift = asReal(s_shift);

  unsigned int clen, sstart, rstart, rcounter;
  rstart = 0;
  for (R_len_t x_read_row = 0; x_read_row < nrow_x; x_read_row++) { // loop thru obs
    /* Rprintf("multires feats: row = %i\n", x_read_row); */
    sstart = 0; // starting index of curve
    rstart = 0; // starting index where we write into row of resmat
    for (R_len_t j = 0; j < n_curves; j++) { // loop thru curves per obs
      clen = curve_lens[j]; // get length of current curve
      /* Rprintf("curve start, len: %i, %i\n", sstart, clen); */
      // now calc all features for current curve and write them to resmat
      c_get_curve_features(x, nrow_x, ncol_x, x_read_row, sstart, clen,  // get current curve segment from x (sstart, clen)
        resmat, nrow_resmat, ncol_resmat, x_read_row, rstart, &rcounter, // write to same row in resmat (at rstart), get write length
        reslev, shift);
      /* Rprintf("rcounter = %i\n", rcounter); */
      sstart += clen; // shift read start point by length of last curve
      rstart += rcounter; // shift write start point by length of last written result
    }
  }
}



