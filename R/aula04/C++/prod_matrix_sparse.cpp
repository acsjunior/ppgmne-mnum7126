#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat prod_mat_sparse (arma::sp_mat A, arma::sp_mat B) {
  arma::sp_mat C = A * B ; 
  return(C) ;
}

