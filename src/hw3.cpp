#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector em(NumericVector y) {
  double p = 0.5;
  double lambda = median(y)-0.1;
  double mu = median(y)+0.1;
  NumericVector delta(100,0.0);
  NumericVector ones(100,1.0);
  double l1 = 0;
  double l2 = 0;
  for (int i = 0; i < 100; i++) {
    l1 = p * lambda * exp(-lambda * y[i]);
    l2 = (1- p) * mu * exp(-mu * y[i]);
    delta[i] = l1/(l1+l2);
  }

  for (int j = 0; j < 1000; j++) {
    lambda = sum(delta)/ sum(delta * y);
    mu = sum(ones - delta)/ sum((ones-delta) * y);
    p = sum(delta)/100;
    for (int i = 0; i < 100; i++) {
      l1 = p * lambda * exp(-lambda * y[i]);
      l2 = (1- p) * mu * exp(-mu * y[i]);
      delta[i] = l1/(l1+l2);
    }
  }

  NumericVector res = NumericVector::create(_["lambda"] = lambda, _["mu"] = mu, _["p"] = p);
  return res;
}


// [[Rcpp::export]]
NumericVector bootsonce(NumericVector y) {
  NumericVector x = sample(y, 100, true);
  NumericVector theta = em(x);
  double p = theta[2];
  double lambda = theta[0];
  double mu = theta[1];

  NumericVector res = NumericVector::create(_["lambda"] = lambda, _["mu"] = mu, _["p"] = p);
  return res;
}
