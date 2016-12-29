normalize = function(x){
  m <- ncol(x)
  for(i in 1:m){
    x[,i] <- (x[,i]-mean(x[,i]))/sd(x[,i]);
  }
  return(x)
}

centerlize = function(x){
  x <- x-mean(x)
  return(x)
}

s = function(beta,r){
  if(r < abs(beta) ){
    if(beta>0)
      beta <- beta-r
    if(beta<0)
      beta <- beta+r
  }else{
    beta<-0
  }
  return(beta)
}

LASSO = function(x,y,k){
  m <- ncol(x)
  n <- nrow(x)
  x <- normalize(x)
  y <- as.matrix(centerlize(y))
  beta <- (solve(t(x)%*%x))%*%(t(x))%*%y
  for(lambda in seq(0,k,0.01)){
    for(i in 1:m){
      beta <- c(beta,s(beta[i]+1/sum(x[,i]^2)*sum(x[,i]*(y-x%*%beta[1:m])),lambda))
    }
  }
  beta <- matrix(beta,nrow=m)
  return(beta[,ncol(beta)])
}
