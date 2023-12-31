#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat escalar_vector_sparse (double alpha, arma::sp_mat y) {
  arma::sp_mat out = alpha * y ;
  return(out) ;
}