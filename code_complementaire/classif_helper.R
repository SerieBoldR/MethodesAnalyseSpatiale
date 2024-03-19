Phi2dist <- function(x){
  all_vals <- do.call(c,x)
  counts <- table(all_vals) / nrow(x)
  div <- ncol(x)
  dist_mat <- matrix(0,ncol = nrow(x), nrow = nrow(x))
  for(i in 1:nrow(dist_mat)){
    for(j in 1:nrow(dist_mat)){
      if(i!=j){
        set <- setdiff(x[i,],x[j,])
        if(length(set) == 0){
          val <- 0
        }else{
          val <- sum(rep(1, length(set)) / counts[match(set, names(counts))])/div
        }
        dist_mat[i,j] <- val
        dist_mat[j,i] <- val
      }
    }
  }
  return(dist_mat)
}
