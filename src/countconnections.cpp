#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix countconnections(IntegerMatrix dm) {
  IntegerMatrix ret(dm.nrow(), dm.nrow());
  
  for(int c = 0; c < dm.ncol(); c++){
    for(int r = 0; r < dm.nrow() - 1; r++){
      for(int q = r + 1; q < dm.nrow(); q++){
       if(dm(r, c) == dm(q, c)){
         ret(r, q) += 1;
       } 
      }
    }
  }
  
  return ret;
}
