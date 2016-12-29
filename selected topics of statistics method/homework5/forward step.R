normalize = function(x) {
  m <- ncol(x)
  for (i in 1:m) {
    x[, i] <- (x[, i] - mean(x[, i])) / sd(x[, i])
  }
  return(x)
}

centerlize = function(x) {
  x <- x - mean(x)
  return(x)
}

fstep = function(x,y,k){
  m <- ncol(x)
  n <- nrow(x)
  x <- normalize(x)
  y <- centerlize(y)
  beta = as.matrix(rep(0,m),nrow=m)
  for(i in 1:100){
    c <- t(x)%*%(y-x%*%beta)
    q <- which(abs(c)==max(abs(c)))
    epsilon <- abs(c[q])/n
    beta[q] <- beta[q] + c[q]/abs(c[q])*epsilon
    if(length(which(beta!=0))==(k+1)){
      beta[q] <- 0
      break
    }
  }
  return(beta)
}