FLDA = function(x,y){
  m <- ncol(x)
  n <- nrow(x)
  miu = matrix(rep(0,(m*max(y))),ncol=m)
  for(i in 1:max(y)){
    miu[i,] <- apply(x[which(y==i),],2,sum)/(length(which(y==i)))
  }
  miu1 = apply(miu,2,sum)
  sb <- matrix(rep(0,m*m),ncol=m)
  for(i in 1:max(y)){
    sb <- sb + (length(which(y==i)))*(miu[i,]-miu1)%*%t(miu[i,]-miu1)
  }
  sw <- matrix(rep(0,m*m),ncol=m)
  for(i in 1:n){
    sw <- sw + (x[i,]-miu[y[i],])%*%t(x[i,]-miu[y[i],])  
  }
  s <- solve(sw)%*%sb
  a <- eigen(s)
  k <- 1
  v<- matrix(rep(0,k*m),nrow=m)
  for(i in 1:k){
    v[,i] <- a$vectors[,i]
  }
  print(v)
  return(list(Re(x%*%v),v))
}
