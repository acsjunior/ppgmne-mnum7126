#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
double angulo_theta_sparse (arma::sp_mat x, arma::sp_mat y) {
  arma::sp_mat temp1 = x.t() * y ;
  arma::sp_mat size_x = sqrt(x.t() * x) ;
  arma::sp_mat size_y = sqrt(y.t() * y) ;
  double out = temp1(0)/(size_x(0) * size_y(0)) ;
  return(out) ;
}