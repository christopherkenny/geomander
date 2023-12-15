#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List localmoran(NumericVector x, NumericMatrix mat) {
  // get constants that will be reused
  int N = x.size();
  double xbar = mean(x);
  NumericVector Ii(N);
  NumericVector z = x - xbar;
  double m2 = sum(pow(z, 2.0))/ (double) N;
  double normc = 0.0;
  NumericVector wi(N);
  NumericVector wi2(N);
  
  double temp, wtemp, w2temp;
  for(int i = 0; i < N; i++){
    temp = 0.0;
    wtemp = 0.0;
    w2temp = 0.0;
    for(int j = 0; j < N; j++){
      temp += mat(i,j)*z(j);
      normc += mat(i,j);
      wtemp += mat(i,j);
      w2temp += pow(mat(i,j), 2.0);
    }
    Ii(i) = z(i)*temp/m2;
    wi(i) = wtemp;
    wi2(i) = w2temp;
  }
  
  // compute variances
  double m4 = sum(pow(z, 4.0))/ (double) N;
  double b2 = m4/(pow(m2, 2.0));
  double n = (double) N;
  NumericVector wikh(N);
  double tempwikh(N);
  for(int i = 0; i < N; i++){
    tempwikh = 0.0;
    for(int k = 0; k < (N - 1); k++){
      for(int h = (k + 1); h < N; h++){
        tempwikh += mat(i, k) * mat(i, h);
      }
    }
    wikh(i) = tempwikh;
  } 
  
  
  NumericVector vari = ((wi2*(n-b2))/(n - 1)) + ((2*wikh*(2*b2 - n))/((n - 1)*(n - 2))) - (wi2/((n - 1)*(n - 1)));
  
  List ret;
  ret["moran"] = Ii;
  ret["expectation"] = (-1*wi)/( (double) N - 1 );
  ret["variance"] = vari;
  
  return ret;
}

// [[Rcpp::export]]
List globalmoran(NumericVector x, NumericMatrix mat) {
  // get constants that will be reused
  int N = x.size();
  double xbar = mean(x);
  NumericVector Ii(x.size());
  NumericVector z = x - xbar;
  double m2 = sum(pow(z, 2.0))/ (double) N;
  double normc = 0.0;

  double temp;
  for(int i = 0; i < N; i++){
    temp = 0.0;
    for(int j = 0; j < N; j++){
      temp += mat(i,j)*z(j);
      normc += mat(i,j);
    }
    Ii(i) = z(i)*temp/m2;
  }
  double moran = sum(Ii)/normc;
  
  List ret;
  ret["moran"] = moran;
  ret["expectation"] = (-1.0)/((double) N - 1.0);

  return ret;
}


