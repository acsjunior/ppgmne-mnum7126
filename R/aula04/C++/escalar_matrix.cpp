#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat escalar_mat (double alpha, arma::mat A) {
  arma::mat B = alpha*A ; 
  return(B) ;
}

