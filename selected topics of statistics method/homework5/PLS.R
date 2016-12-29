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

PLS = function(x,y,g){
  m <- ncol(x)
  n <- nrow(x)
  x <- normalize(x)
  y <- as.matrix(centerlize(y))
  W <- matrix(rep(0,m*g),ncol=g)
  Tg <- matrix(rep(0,n*g),ncol=g)
  P <- matrix(rep(0,m*g),ncol=g)
  C <- rep(0,g)
  for (j in 1:g){
    W[,j] <- t(x) %*% y/ sum(abs(t(x)%*%y))
    Tg[,j] <- x %*% W[,j]
    a <- t(Tg[,j])%*%Tg[,j]
    C[j] <- (t(Tg[,j]) %*% y) / a
    P[,j] <- (t(x) %*% Tg[,j]) / rep(a,m)
    x <- x-Tg[,j]%*%t(P[,j])
    y <- y - Tg[,j] * C[j]
  }
  beta <- W %*% as.matrix(C)
  return(beta)
}
