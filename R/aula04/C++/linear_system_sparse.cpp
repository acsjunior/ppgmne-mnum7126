// Important: this definition ensures Armadillo enables SuperLU
#define ARMA_USE_SUPERLU 1

#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]

using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
arma::vec linear_system_sparse (arma::sp_mat A, arma::vec B) {
  arma::vec C = spsolve(A, B) ;
  return(C) ;
}

