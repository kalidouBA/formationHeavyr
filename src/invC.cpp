#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
//'Compute inverse matrix in c++
//'@param A a matrix
//'
//'@export
// [[Rcpp::export]]
arma::mat invC(arma::mat A){
  arma::mat Ainv = inv(A);
  return Ainv;
}
