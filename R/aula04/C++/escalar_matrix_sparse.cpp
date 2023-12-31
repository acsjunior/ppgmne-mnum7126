#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat escalar_mat_sparse (double alpha, arma::sp_mat A) {
  arma::sp_mat B = alpha*A ; 
  return(B) ;
}

