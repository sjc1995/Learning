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

ridge_regression = function(x,y,lambda){
  m <- ncol(x)
  n <- nrow(x)
  x <- normalize(x)
  y <- centerlize(y)
  beta <- rep(0,(m))
  sigma <- 0
  beta1 <- (solve(t(x)%*%x+lambda*diag(m)))%*%(t(x))%*%y
  sigma1 <- sum((y-x%*%beta)^2)
  if(sum(abs(beta-beta1)/m) < 0.001 && abs(sigma-sigma1)/n<0.001){
    sigma <- sigma1
    beta <- beta1
    break
  }else{
    sigma <- sigma1
    beta <- beta1
  }
  return(beta)
}

lq = function(x,y){
  n <- length(y)
  x <- normalize(x)
  y <- centerlize(y)
  beta <- (solve(t(x)%*%x))%*%(t(x))%*%y
  return(beta)
}

