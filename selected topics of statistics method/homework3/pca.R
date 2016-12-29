pca = function(data,k){
  n <- nrow(data)
  m <- ncol(data)
  for(i in 1:m){
   data[,i] <- data[,i]-mean(data[,i]) 
  }
  cov <- t(data)%*%data
  a <- eigen(cov)
  P <- matrix(rep(0,k*m),nrow=m)
  for(i in 1:k){
    P[,i] <- a$vectors[,i]
  }
  X <- data%*%P
  h <- sum(a$values[1:k])/sum(a$values)
  return(list(X,P,h))
}

pr = function(X){
  n <- ncol(X)
  if(n==1){
    plot(X,rep(0,nrow(X)))
  }
  if(n==2){
    plot(X[,1],X[,2])
  }
}
