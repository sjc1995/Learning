nmf = function(data,k){
  m <- ncol(data)
  n <- nrow(data)
  W <- matrix(abs(rnorm(n*k)),ncol=k)
  H <- matrix(abs(rnorm(k*m)),ncol=m)
  a <- sum(data^2)
  for(i in 1:10000){
    epsilon <-sum((data-W%*%H)^2)
    if(epsilon/a<0.01){
      break
    }else{
        H <- H*(t(W)%*%data)/((t(W)%*%W%*%H)+10^(-9))
        W <- W*(data%*%t(H))/((W%*%H%*%t(H))+10^(-9))
      }
  }
  return(list(W,H,epsilon/a))
}

nmf2 = function(data,k){
  m <- ncol(data)
  n <- nrow(data)
  W <- matrix(abs(rnorm(n*k)),ncol=k)
  H <- matrix(abs(rnorm(k*m)),ncol=m)
  for(i in 1:1000){
    epsilon <-sum(data*log(data/(W%*%H)+10^(-9))-data+W%*%H)
    if(epsilon<0.01 || is.na(epsilon)){
      break
    }else{
      H <- H*t((t(data/(W%*%H+10^(-9)))%*%W))/matrix(rep(apply(W,2,sum),m),ncol=m,byrow=F)
      W <- W*t((H%*%t(data/(W%*%H+10^(-9)))))/matrix(rep(apply(H,1,sum),n),ncol=k,byrow=T)
    }
  }
  return(list(W,H,epsilon))
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
