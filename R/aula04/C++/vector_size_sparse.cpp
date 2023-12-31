#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat vector_size_sparse (arma::sp_mat x) {
  arma::sp_mat out = x.t() * x ;
  arma::sp_mat size = sqrt(out) ;
  return(size) ;
}