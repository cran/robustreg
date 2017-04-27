// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// fit_rcpp
arma::mat fit_rcpp(arma::mat X, arma::vec b);
RcppExport SEXP robustreg_fit_rcpp(SEXP XSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(fit_rcpp(X, b));
    return rcpp_result_gen;
END_RCPP
}
// median_rcpp
double median_rcpp(NumericVector x);
RcppExport SEXP robustreg_median_rcpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(median_rcpp(x));
    return rcpp_result_gen;
END_RCPP
}
// mad_rcpp
double mad_rcpp(NumericVector r, double scale_factor);
RcppExport SEXP robustreg_mad_rcpp(SEXP rSEXP, SEXP scale_factorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type scale_factor(scale_factorSEXP);
    rcpp_result_gen = Rcpp::wrap(mad_rcpp(r, scale_factor));
    return rcpp_result_gen;
END_RCPP
}
// psiBS_rcpp
NumericVector psiBS_rcpp(NumericVector r, double c);
RcppExport SEXP robustreg_psiBS_rcpp(SEXP rSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(psiBS_rcpp(r, c));
    return rcpp_result_gen;
END_RCPP
}
// psiHuber_rcpp
NumericVector psiHuber_rcpp(NumericVector r, double c);
RcppExport SEXP robustreg_psiHuber_rcpp(SEXP rSEXP, SEXP cSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type r(rSEXP);
    Rcpp::traits::input_parameter< double >::type c(cSEXP);
    rcpp_result_gen = Rcpp::wrap(psiHuber_rcpp(r, c));
    return rcpp_result_gen;
END_RCPP
}
