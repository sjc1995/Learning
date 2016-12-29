f = function(x,y){
  return((1-x)^2+100*(y-x^2)^2)
}

d = function(h,x){
  delta = 10^(-16)
  a = (h(x[1]+delta,x[2])-h(x[1]-delta,x[2]))/(2*delta)
  b = (h(x[1],x[2]+delta)-h(x[1],x[2]-delta))/(2*delta)
  return(c(a,b))
}

df1=deriv(z~(1-x)^2+100*(y-x^2)^2,c("x","y"),func = TRUE,hessian = TRUE)

      
sd = function(){
  x = matrix(nrow=1000,ncol=2)
  x[1,] = runif(2,-2,2)
  for(i in 1:1000){
    x[i+1,] = x[i,]- attr(df1(x[i,1],x[i,2]),'gradient')%*%solve(attr(df1(x[i,1],x[i,2]),'hessian')[1,,])
    print(x[i+1,])
    if(abs(f(x[(i+1),1],x[(i+1),2]))<10^(-16))
      break
  }
  print(i)
  cat(f(x[i+1,1],x[i+1,2]))
  return(x[i+1,])
}

sd()
