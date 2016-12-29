f = function(x,alpha){
  y = alpha * x * (1-x)
  return(y)
}

logistic = function(alpha){
  x = rep(0,3000)
  x[1] = runif(1)
  for(i in 1:2999){
    x[(i+1)] = f(x[i],alpha)
  }
  return (x[1001:3000])
}

result = function(){
  for(alpha in seq(0,4,0.01)){
    x = logistic(alpha)
    plot(rep(alpha,2000),x,xlim=c(-0.1,4.1),ylim=c(-0.1,1.1),xlab="alpha",ylab="x")
    par(new=TRUE,cex=0.01)
  }
}

result()

