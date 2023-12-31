#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec escalar_vector (double alpha, arma::vec y) {
  arma::vec out = alpha * y ;
  return(out) ;
}