wioa <- function (obs,sim) {
  d <- 1 -  (sum((obs - sim)^2)   / sum( ( abs(sim - mean(obs)) + abs(obs - mean(obs)) )^2 ))
  d <- c(d,mean(abs(obs-sim)/abs(obs)))
  d
}
