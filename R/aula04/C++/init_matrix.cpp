#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat init_diag (int linha, int coluna) {
  arma::mat A = eye(linha, coluna) ; 
  return(A) ;
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat init_um (int linha, int coluna) {
  arma::mat A = ones(linha, coluna) ; 
  return(A) ;
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat init_zero (int linha, int coluna) {
  arma::mat A = zeros(linha, coluna) ; 
  return(A) ;
}

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::mat minha_funcao (int linha, int coluna) {
  arma::mat A = init_zero(linha, coluna) ;
  arma::mat B = init_um(linha, coluna) ;
  arma::mat C = randu(linha, coluna) ;
  arma::mat D = A + B + C ;
  return(C) ;
}
