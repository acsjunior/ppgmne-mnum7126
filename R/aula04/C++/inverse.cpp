#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat inverse (arma::mat A) {
  arma::mat C = inv(A) ; 
  return(C) ;
}

