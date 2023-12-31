#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat produto_interno_sparse (arma::sp_mat x, arma::sp_mat y) {
  arma::sp_mat out = x.t() * y ;
  return(out) ;
}