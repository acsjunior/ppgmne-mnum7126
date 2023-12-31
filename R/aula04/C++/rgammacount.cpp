#include <math.h>
#include <R.h>
#include <Rmath.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = ".qgcnt_one")]]
int qgcnt_one(double p,
              double lambda,
              double alpha) {
  double alphalambda;
  alphalambda = 1/(alpha * lambda);
  int x;
  x = 0;
  double gl, gr, px, Px;
  gl = 1;
  gr = R::pgamma(1, alpha, alphalambda, 1, 0);
  Px = px = gl - gr;
  while (Px < p) {
    gl = gr;
    x = x + 1;
    gr = R::pgamma(1, (x + 1) * alpha, alphalambda, 1, 0);
    px = gl - gr;
    Px = Px + px;
  }
  return x;
}

// [[Rcpp::export(name = ".rgcnt_one")]]
int rgcnt_one(double lambda,
              double alpha) {
  double u;
  u = R::runif(0.0, 1.0);
  int x;
  x = qgcnt_one(u, lambda, alpha);
  return x;
}