// [[Rcpp::export]]
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
RObject callWithOne(Function f, double n) {
  return f(n);
}