lq = function(x,y){
  n <- length(y)
  x <- matrix(c(rep(1,n),x),nrow=n)
  beta <- (solve(t(x)%*%x))%*%(t(x))%*%y
  return(beta)
}

tt = function(a,beta,x,y,alpha=0.1,beta0=0){
  n <- length(y)
  x <- matrix(c(rep(1,n),x),nrow=n)
  SSE <- sum((y-x%*%beta)^2)
  sigma = SSE/(n-2)
  sxx <- sum(x[,2]^2)-n*(mean(x[,2]))^2
  var <- c(0,0)
  var[2] <- sigma/sxx
  var[1] <- sigma*(1/n+(mean(x[,2]))^2/sxx)
  tt <- qt(1-alpha/2,n-2)
  print((beta[a+1]-beta0)/sqrt(var[a+1]))
  if (abs(beta[a+1]-beta0)/sqrt(var[a+1])<tt){
    return("yes")
  }else{
    return("wrong")
  }
}

ff = function(a,beta,x,y,alpha=0.05,beta0=0){
  n <- length(y)
  x <- matrix(c(rep(1,n),x),nrow=n)
  SSEF <- sum((y-x%*%beta)^2)
  xr = x
  xr[,(a+1)]=0
  SSER <- sum((y-xr%*%beta)^2)
  lena <- length(a)
  ff <- ((SSER-SSEF)/lena)/(SSEF/(n-length(beta)))
  print(ff)
  if (abs(ff-beta0) < qf(1-alpha,length(a),(n-length(beta)))){
    return("yes")
  }else{
    return("wrong")
  }
}
