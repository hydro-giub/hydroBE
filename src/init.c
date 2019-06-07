#include <stdlib.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "runSum.h"

static const R_CallMethodDef callRoutines[]  = {
  {"runSum", (DL_FUNC) &runSum, 2},
  {NULL, NULL, 0}
};

void R_init_hydroBE(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, callRoutines, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
