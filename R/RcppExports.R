# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

fit_rcpp <- function(X, b) {
    .Call(`_robustreg_fit_rcpp`, X, b)
}

median_rcpp <- function(x) {
    .Call(`_robustreg_median_rcpp`, x)
}

mad_rcpp <- function(r, scale_factor = 1.4826) {
    .Call(`_robustreg_mad_rcpp`, r, scale_factor)
}

psiBS_rcpp <- function(r, c) {
    .Call(`_robustreg_psiBS_rcpp`, r, c)
}

psiHuber_rcpp <- function(r, c) {
    .Call(`_robustreg_psiHuber_rcpp`, r, c)
}

