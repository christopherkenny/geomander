#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix adjlist2matrix(List adj) {
  NumericMatrix mat(adj.size(), adj.size());
  IntegerVector temp;
  
  for(int i = 0; i < adj.size(); i++){
    temp = adj(i);
    for(int j = 0; j < temp.size(); j++){
      mat(i, temp(j)) = 1;
    }
  }
  
  return mat;
}


