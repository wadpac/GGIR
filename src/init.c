#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP _GGIR_numUnpack(SEXP);
extern SEXP _GGIR_resample(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_GGIR_numUnpack", (DL_FUNC) &_GGIR_numUnpack, 1},
    {"_GGIR_resample",  (DL_FUNC) &_GGIR_resample,  5},
    {NULL, NULL, 0}
};

void R_init_GGIR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
