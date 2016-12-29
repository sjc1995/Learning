pca = function(data,k){
  n <- nrow(data)
  m <- ncol(data)
  for(i in 1:m){
    data[,i] <- (data[,i]-mean(data[,i]))/sd(data[,i]) 
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

pcr = function(x,y,k){
  T1 <- as.matrix(pca(x,k)[[1]])
  C <- solve(t(T1) %*% T1)%*%t(T1)%*%y
  beta <- as.matrix(pca(x,k)[[2]]) %*% as.matrix(C)
  return(beta)
}