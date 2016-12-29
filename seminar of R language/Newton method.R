f = function(x){
  return(log(x)-exp(-x))
}

Newton = function(){
  x = runif(1)
  for(i in 1:1000){
    x = x - f(x) /((f(x+10^(-6))-f(x-10^(-6)))/(2*10^(-6)))
    if(abs(f(x)) < (10^(-16)))
      break
  }
  print(i)
  return(c(x,f(x)))
}

Newton()