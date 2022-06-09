#include <Rcpp.h>
using namespace Rcpp;
//'Compute
//'@param x a vector
//'@export
// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x){
  return x * 2;
}
