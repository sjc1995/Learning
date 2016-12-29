f = function(x){
  if (length(x)<=1){
    return(x)
  }else{
    key <- x[1]
    left <- x[which(x<key)]
    right <- x[which(x>key)]
    left <- f(left)
    right <- f(right)
    x <- c(left,key,right)
    return(x)
  }
}

a <- runif(20,0,10)

f(a)