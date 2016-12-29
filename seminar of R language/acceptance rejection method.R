set.seed(21)
s = function(n){
  x = rep(0,n)
  for(i in 1:n){
    for(j in 1:10000){
      x[i] = runif(1,0,2)
      y = runif(1)
      if(x[i]<1){
        if(y < x[i]){
          break
      }else if(x[i] >= 1){
        if(y < (2-x[i])){
          break
        }
      }
    }
  }
  return(x)
}

z = s(3000)
p = rep(0,20)
for(i in 1:20){
  p[i]=length(which(z<(0.1*i)&z>(0.1*(i-1))))/3000
}
print(p)
barplot(p)