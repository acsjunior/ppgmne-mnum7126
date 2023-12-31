#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec vector_size (arma::vec x) {
  arma::vec out = x.t() * x ;
  arma::vec size = sqrt(out) ;
  return(size) ;
}