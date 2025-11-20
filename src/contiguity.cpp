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

// [[Rcpp::export]]
IntegerMatrix contiguity_matrix(List adj, IntegerMatrix group) {
  int n = group.nrow();
  int n_plans = group.ncol();
  IntegerMatrix results(n, n_plans);
  
  for(int plan = 0; plan < n_plans; plan++){
    IntegerVector curr_group = group(_, plan);
    IntegerVector conncomp = contiguity(adj, curr_group);
    results(_, plan) = conncomp;
  }
  
  return results;
}

// [[Rcpp::export]]
bool is_contiguous(List adj, IntegerVector group) {
  int n = group.size();
  std::vector<int> conncomp(n, 0);
  
  int max_group = max(group);
  std::vector<int> group_cc(max_group + 1, 0);
  
  std::queue<int> q;
  
  for(int i = 0; i < n; i++){
    if(conncomp[i] == 0){
      int curr_group = group[i];
      group_cc[curr_group]++;
      
      if(group_cc[curr_group] > 1){
        return false;
      }
      
      int cc = group_cc[curr_group];
      
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
  
  return true;
}

// [[Rcpp::export]]
LogicalVector is_contiguous_mat(List adj, IntegerMatrix group) {
  int n_plans = group.ncol();
  LogicalVector results(n_plans);
  
  for(int plan = 0; plan < n_plans; plan++){
    IntegerVector curr_group = group(_, plan);
    results[plan] = is_contiguous(adj, curr_group);
  }
  
  return results;
}