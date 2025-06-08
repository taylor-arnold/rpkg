#define STRICT_R_HEADERS

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R.h>
#include <Rinternals.h>

#include "ctest.h"

static R_NativePrimitiveArgType pkstwo_type[] = {
    INTSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType psmirnov2x_type[] = {
    REALSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType pkolmogorov2x_type[] = {
    REALSXP, INTSXP
};

static const R_CMethodDef R_CDef[] = {
  {"pkstwo", (DL_FUNC) &pkstwo, 3, pkstwo_type},
  {"psmirnov2x", (DL_FUNC) &psmirnov2x, 3, psmirnov2x_type},
  {"pkolmogorov2x", (DL_FUNC) &pkolmogorov2x, 2, pkolmogorov2x_type},
   {NULL, NULL, 0, NULL}
};

void R_init_dgof(DllInfo *dll)
{
  R_registerRoutines(dll, R_CDef, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
