largest_intersection_geos <- function(x, y) {
  l <- geos::geos_intersects_matrix(x, y) 
  
  a <- lapply(seq_along(l), function(i) {
    sapply(l[[i]], function(j) {
      geos::geos_area(geos::geos_intersection(x[i, ], y[j, ]))
    })
  })
  
  vapply(seq_along(l), function(i) {
    o <- l[[i]][which.max(a[[i]])]
    if (length(o) == 0) {
      o <- NA_real_
    }
    o
  }, 0)
  
}