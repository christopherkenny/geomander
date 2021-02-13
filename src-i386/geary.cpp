#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector localgeary(NumericVector x, NumericMatrix mat) {
  int N = x.size();
  double n = (double) N;
  NumericVector ci(N);
  double xbar = mean(x);
  NumericVector z = x - xbar;
  double m2 = sum(pow(z, 2.0))/ n;
  
  double temp;
  for(int i = 0; i < N; i++){
    temp = 0.0;
    for(int j = 0; j < N; j++){
      temp += mat(i,j) * pow(z(i) - z(j), 2.0);
    }
    ci(i) = temp;
  }

  return ci/m2;
}


// [[Rcpp::export]]
double globalgeary(NumericVector x, NumericMatrix mat) {
  int N = x.size();
  double n = (double) N;
  NumericVector ci(N);
  double xbar = mean(x);
  NumericVector z = x - xbar;
  double z2 = sum(pow(z, 2.0));
  double S0 = 0.0;
  double temp;
  
  
  for(int i = 0; i < N; i++){
    temp = 0.0;
    for(int j = 0; j < N; j++){
      temp += mat(i,j) * pow(z(i) - z(j), 2.0);
      S0 += mat(i, j);
    }
    ci(i) = temp;
  }
  double c = sum(ci)/z2;
  
  c = c * (n - 1.0);
  c = c / 2;
  c = c/S0;
  
  return c;
}
