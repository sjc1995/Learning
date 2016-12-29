limit = function(alpha,L,H){
  if(alpha<L){
    alpha=L
  }else if(alpha>H){
    alpha=H
  }
  return(alpha)
}

sele= function(i,num){
  j <- sample(1:num,1)
  if(j==i){
    j<- sele(i,num)
  }
  return(j)
}

svm= function(x,y){
  b<- 0
  c <- 1
  alpha <- c(rep(0,200))
  for(q in 1:10000){
    w <- c(sum(alpha*y*x[,1]),sum(alpha*y*x[,2]))
    E1 <- c(rep(0,200))
    E2 <- c(rep(0,200))
    for(i in 1:200){
      if((w%*%x[i,]+b-y[i])*y[i]<=0){
        E1[i] <- w%*%x[i,]+b-y[i]
      }else{
        E2[i] <- w%*%x[i,]+b-y[i]
      }
    }
    if(max(abs(E1))<=0.01){
      break
    }else{
      i<-which(abs(E1)==max(abs(E1)))[1]
      j<-sele(i,200)
      E <- E1+E2
      oldalphai<- alpha[i]
      oldalphaj<- alpha[j]
      if(y[i]*y[j]<0){
        L=max((alpha[j]-alpha[i]),0)
        H=min(c,(c+alpha[j]-alpha[i]))
      }else if(y[i]*y[j]>0){
        L=max((alpha[j]+alpha[i]-c),0)
        H=min(c,(alpha[j]+alpha[i]))
      }
      eta<-2*x[i,]%*%x[j,]-x[i,]%*%x[i,]-x[j,]%*%x[j,]
      alpha[j]<-alpha[j]-y[j]*(E[i]-E[j])/eta
      alpha[j]<-limit(alpha[j],L,H)
      alpha[i]<-alpha[i]+y[i]*y[j]*(oldalphaj-alpha[j])
      b1<--E[i]+y[i]*(alpha[i]-oldalphai)*(x[i,]%*%x[i,])-y[j]*(alpha[j]-oldalphaj)*(x[i,]%*%x[j,])+b
      b2<--E[j]+y[j]*(alpha[i]-oldalphai)*(x[i,]%*%x[j,])-y[j]*(alpha[j]-oldalphaj)*(x[j,]%*%x[j,])+b
      if(0>alpha[i] || c<alpha[i]){
        b=b1
      }else if(0<alpha[i] & c>alpha[i]) {
        b=b2
      }else{
        b=(b1+b2)/2
      }
    }
  }
  return(c(w,b))
}
