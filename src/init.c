#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP GGIR_numUnpack(SEXP);
extern SEXP GGIR_resample(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"GGIR_numUnpack", (DL_FUNC) &GGIR_numUnpack, 1},
    {"GGIR_resample",  (DL_FUNC) &GGIR_resample,  4},
    {NULL, NULL, 0}
};

void R_init_GGIR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
