#' Count Times Precincts are Connected
#'
#' @param dm district membership matrix 
#' @param normalize Whether to normalize all values by the number of columns.
#'
#' @return matrix with the number of connections between precincts
#' @export
count_connections <- function(dm, normalize = FALSE){
  mat <- countconnections(dm)
  mat <- mat + t(mat)
  diag(mat) <- rep(ncol(dm), nrow(dm))
  
  if(normalize){
    mat <- mat/ncol(dm)
  }
  
  out <- expand.grid(1:nrow(dm), 1:nrow(dm))
  names(out) <- c('x','y')
  out <- out %>% mutate(fill = c(mat))
  return(out)
}