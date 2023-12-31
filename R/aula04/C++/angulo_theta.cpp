#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec angulo_theta (arma::vec x, arma::vec y) {
  arma::vec temp1 = x.t() * y ;
  arma::vec size_x = sqrt(x.t() * x) ;
  arma::vec size_y = sqrt(y.t() * y) ;
  arma::vec out = temp1/(size_x * size_y) ;
  return(out) ;
}