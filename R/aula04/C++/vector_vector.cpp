#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::vec vector_vector (arma::vec x, arma::vec y) {
  arma::vec out = x + y ;
  return(out) ;
}