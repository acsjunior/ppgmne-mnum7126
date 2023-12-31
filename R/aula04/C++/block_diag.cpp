#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat block_mat (arma::sp_mat A, int p, int q) {
  arma::sp_mat B = repmat(A, p, q) ; 
  return(B) ;
}

