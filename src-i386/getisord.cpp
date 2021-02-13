#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector localgstar(NumericVector x, NumericMatrix mat) {
  int N = x.size();
  double n = (double) N;
  NumericVector Gi(N);
  NumericVector Wi = rowSums(mat);
  NumericVector Wi2 = pow(Wi, 2.0);
  NumericVector VarGi(N);
  NumericVector x2 = pow(x, 2.0);
  
  double xbar = mean(x);
  double s2 = sum(x2)/(n) - pow(xbar, 2.0);
  VarGi = (s2/xbar)*((Wi*(n - Wi))/(n-1));
  double s = pow(s2, 0.5);
  
  NumericVector num(N);
  NumericVector denom(N);
  double tempn, tempd;
  
  for(int i = 0; i < N; i++){
    tempn = 0.0;
    tempd = 0.0;
    for(int j = 0; j < N; j++){
     tempn += (mat(i,j)*x(j) - xbar *mat(i,j));
      tempd += pow(mat(i,j) ,2.0);
    }
    num(i) = tempn;
    tempd = (n*tempd - Wi2(i))/(n - 1.0);
    denom(i) = s*pow(tempd, 0.5);
  }
  
  Gi = num/denom;
  
  return Gi;
}


