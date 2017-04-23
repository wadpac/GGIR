#include <stdlib.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>  // optional

#include "GGIR.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  CALLDEF(GGIR_numUnpack, 1),
  CALLDEF(GGIR_resample, 4),
  {NULL, NULL, 0}
};

void
  attribute_visible  // optional
  R_init_GGIR(DllInfo *dll)
  {
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
  }
