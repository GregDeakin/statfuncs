comb <- function(n,r) {
  if(r==0) return(1)
  prod(((1+n-r):n)/(1:r))
}
