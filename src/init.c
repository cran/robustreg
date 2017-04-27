#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP robustreg_fit_rcpp(SEXP, SEXP);
extern SEXP robustreg_mad_rcpp(SEXP, SEXP);
extern SEXP robustreg_median_rcpp(SEXP);
extern SEXP robustreg_psiBS_rcpp(SEXP, SEXP);
extern SEXP robustreg_psiHuber_rcpp(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"robustreg_fit_rcpp",      (DL_FUNC) &robustreg_fit_rcpp,      2},
    {"robustreg_mad_rcpp",      (DL_FUNC) &robustreg_mad_rcpp,      2},
    {"robustreg_median_rcpp",   (DL_FUNC) &robustreg_median_rcpp,   1},
    {"robustreg_psiBS_rcpp",    (DL_FUNC) &robustreg_psiBS_rcpp,    2},
    {"robustreg_psiHuber_rcpp", (DL_FUNC) &robustreg_psiHuber_rcpp, 2},
    {NULL, NULL, 0}
};

void R_init_robustreg(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
