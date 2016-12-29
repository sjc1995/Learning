f = function(x){
  y <- x^3-7*x^2+1
  return(y)
}

trape = function(x){
  f <- (1/2)*(sum(f(x[1:(length(x)-1)]))+sum(f(x[2:length(x)])))*(max(x)-min(x))/(length(x)-1)
  return(f)
}

simpson = function(x){
  sim <- (max(x)-min(x))/(length(x)-1)/3*(f(x[1])+f(x[length(x)])+sum(2*f(x[2:(length(x)-1)]))+sum(2*f(x[which(c(1:length(x))%%2==0)])))
  return(sim)
}

mc = function(a,b,c,d,q){
  xx <- a+runif(q)*(b-a)
  yy <- c+runif(q)*(d-c)
  xxx <- f(xx)
  E <- length(which(yy<xxx))/q
  f <- E*(b-a)*(d-c)+c*(b-a)
  return(f)
}

leb = function(n,a,b){
  xx <- seq(a,b,1/n)
  I <- 1/n*sum((xx^3-7*xx^2+1)*(b-a))
  return(I)
}

x <- seq(0,1,0.001)

trape(x)
simpson(x)
mc(0,1,min(f(x)),max(f(x)),100000)
leb(1000,0,1)


