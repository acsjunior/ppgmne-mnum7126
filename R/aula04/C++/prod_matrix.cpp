#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat prod_mat (arma::mat A, arma::mat B) {
  arma::mat C = A * B ; 
  return(C) ;
}

