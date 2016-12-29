fstage = function(x,y,k){
  m <- ncol(x)
  n <- nrow(x)
  epsilon = 0.000001
  beta = as.matrix(rep(0,m),nrow=m)
  for(i in 1:500000){
    c <- t(x)%*%(y-x%*%beta)
    q <- which(abs(c)==max(abs(c)))
    beta[q] <- beta[q] + c[q]/abs(c[q])*epsilon
    if(length(which(beta!=0))==(k+1)){
      beta[q] <- 0
      break
    }
  }
  return(beta)
}