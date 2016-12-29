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

positive = function(x){
  if(max(x)<=0){
    a <-0
  }else{
    a <- x[which(x==min(x[which(x>0)]))]
  }
  return(a)
}


LARS = function(x,y){
  m <- ncol(x)
  n <- nrow(x)
  x <- normalize(x)
  y <- as.matrix(centerlize(y))
  miu <- as.matrix(rep(0,n))
  j <- c()
  s <- c()
  plot(c(-1,5),c(-1,5))
  for(k in 1:(m-1)){
    c <- t(x) %*% (y - miu)
    C <- max(abs(c))
    j <- c(j,(setdiff(which(abs(c) == max(abs(c))),j)))
    s <- c(s,(c[j[length(j)]]/abs(c[j[length(j)]])))
    print(s)
    x1 <- x[,j]
    print(x1)
    x1 <- x1 * matrix(rep(s,n),nrow=n,byrow=T)
    g <- t(x1) %*% x1
    A <- (t(as.matrix(rep(1,length(j)))) %*% solve(g) %*% as.matrix(rep(1,length(j))))^(-1/2)
    w <- A[,] * solve(g) %*% as.matrix(rep(1,length(j)))
    u <- x1 %*% w
    a <- t(x) %*% u
    q1 <- c()
    q2 <- c()
    for(i in setdiff(1:m,j)){
      q1 <- c(q1,(C-c[i])/(A-a[i]))
      q2 <- c(q2,(C+c[i])/(A+a[i]))
    }
    r <- positive(c(q1,q2))
    miu <- miu + r*u
    beta <- (solve(t(x) %*% x)) %*% t(x) %*% miu
  }
  beta <- (solve(t(x) %*% x)) %*% t(x) %*% miu
  return(beta)
}
