#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP GGIR_numUnpack(SEXP pack);
extern SEXP GGIR_resample(SEXP raw, SEXP rawTime, SEXP time, SEXP stop);

static const R_CallMethodDef CallEntries[] = {
    {"GGIR_numUnpack", (DL_FUNC) &GGIR_numUnpack, 1},
    {"GGIR_resample",  (DL_FUNC) &GGIR_resample,  4},
    {NULL, NULL, 0}
};

void R_init_GGIR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
