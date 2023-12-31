#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat vector_vector_sparse (arma::sp_mat x, arma::sp_mat y) {
  arma::sp_mat out = x + y ;
  return(out) ;
}