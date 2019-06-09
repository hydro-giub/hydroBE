#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>

// running sum
// x: numeric vector
// w: positive integer, window width
SEXP runSum(SEXP x, SEXP w) {

  int i, j;
  int nx = length(x);
  int nw = INTEGER(w)[0];

  SEXP a = PROTECT(allocVector(REALSXP,nx));
  double *ap = REAL(a);
  double *xp = REAL(x);

  memset(ap, 0, nx*sizeof(double));
  for(i = 0; i < (nw-1); i++)
    ap[i] = NA_REAL;

  for(i = nw-1; i < nx; i++)
    for(j = i-nw+1; j <= i; j++)
      ap[i] += xp[j];

  UNPROTECT(1);
  return(a);

}


