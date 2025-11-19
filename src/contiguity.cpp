#include <Rcpp.h>
#include <vector>
#include <queue>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector contiguity(List adj, IntegerVector group) {
  int n = group.size();
  IntegerVector conncomp(n);
  
  // Convert group to 0-indexed for faster array access
  int max_group = max(group);
  std::vector<int> group_cc(max_group + 1, 0);
  
  // Pre-allocate queue for BFS
  std::queue<int> q;
  
  for(int i = 0; i < n; i++){
    if(conncomp[i] == 0){
      int curr_group = group[i];
      group_cc[curr_group]++;
      int cc = group_cc[curr_group];
      
      // BFS using queue
      conncomp[i] = cc;
      q.push(i);
      
      while(!q.empty()){
        int curr = q.front();
        q.pop();
        
        IntegerVector neighbors = adj[curr];
        int n_neighbors = neighbors.size();
        
        for(int j = 0; j < n_neighbors; j++){
          int neighbor = neighbors[j];
          if(group[neighbor] == curr_group && conncomp[neighbor] == 0){
            conncomp[neighbor] = cc;
            q.push(neighbor);
          }
        }
      }
    }
  }
  
  return conncomp;
}