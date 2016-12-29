logistic_regression = function(x,g){
  m <- ncol(x)
  n <- nrow(x)
  k <- length(levels(factor(g)))
  beta <- c(rep(0,((k-1)*(m+1))))
  xx = matrix(rep(0,n*(k-1)*(k-1)*(m+1)),nrow=n*(k-1))
  w = matrix(rep(0,n*(k-1)*(k-1)*n),nrow=n*(k-1))
  y <- rep(0,n*(k-1))
  for(i in 1:(k-1)){
    for(j in 1:n){
      if(g[j]==i){
        y[(i-1)*n+j] = 1
      }
    }
  }
  x1 <- matrix(c(rep(1,n),x),nrow = n)
  for(i in 1:(k-1)){
    xx[((i-1)*n+1):(i*n),((i-1)*(m+1)+1):(i*(m+1))] <- x1
  }
  p <-rep(0,(n*(k-1)))
  for(a in 1:100){
    for(i in 1:(k-1)){
      for(j in 1:n){
        s <- 0
        for(d in 1:(k-1)){
          s <- s + exp(sum(c(1,x[j,])*beta[((d-1)*(m+1)+1):(d*(m+1))]))
        }
        p[(i-1)*n+j] =  (exp(sum(c(1,x[j,])*beta[((i-1)*(m+1)+1):(i*(m+1))])))/(1+s)
        print(sum(c(1,x[j,])*beta[((i-1)*(m+1)+1):(i*(m+1))]))
      }
      
      for(j in 1:(k-1)){
        if(i == j){
          w[((i-1)*n+1):(i*n),((i-1)*n+1):(i*n)]=diag(p[((i-1)*n+1):(i*n)]*(1-p[((i-1)*n+1):(i*n)]))
        }else if(i != j){
          w[((i-1)*n+1):(i*n),((j-1)*n+1):(j*n)]=diag(-p[((i-1)*n+1):(i*n)]*p[((j-1)*n+1):(j*n)])
        }
      }
    }
    beta = beta + solve((t(xx)%*%w%*%xx))%*%t(xx)%*%(y-p)
    z <- xx%*%beta
    p[which(p<1)]=0
    for(j in 1:n){
      p[((which(p[c(0:(k-2))*n+j]==max(p[c(0:(k-2))*n+j]))-1)*n+j)]=1
      p[((which(p[c(0:(k-2))*n+j]!=max(p[c(0:(k-2))*n+j]))-1)*n+j)]=0
    }
    if(sum(p-y)/(n*(k-1))<0.01){
      break
    }
    print(a)
  }
  return(beta)
}

