#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat inverse (arma::sp_mat A) {
  arma::sp_mat C = inv(A) ; 
  return(C) ;
}

