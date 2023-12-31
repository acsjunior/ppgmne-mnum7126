#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat linear_system (arma::mat A, arma::mat B) {
  arma::mat C = solve(A, B) ; 
  return(C) ;
}

