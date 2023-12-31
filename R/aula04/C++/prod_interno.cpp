#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec produto_interno (arma::vec x, arma::vec y) {
  arma::vec out = x.t() * y ;
  return(out) ;
}