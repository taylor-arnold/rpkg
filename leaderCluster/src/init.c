#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include <R.h>
#include <Rinternals.h>

#include "ctest.h"

static R_NativePrimitiveArgType leader_cluster_type[] = {
    REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static const R_CMethodDef R_CDef[] = {
  {"leader_cluster", (DL_FUNC) &leader_cluster, 8, leader_cluster_type},
  {NULL, NULL, 0, NULL}
};

void R_init_leaderCluster(DllInfo *dll)
{
  R_registerRoutines(dll, R_CDef, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
