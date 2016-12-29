x = matrix(rep(0,50),ncol=5)
x[1,1:3]=c(1,3,5)
x[2,1:2]=c(2,4)
x[3,1:2]=c(2,3)
x[4,1:4]=c(1,2,3,4)
x[5,1:2]=c(1,2)
x[6,1:2]=c(2,3)
x[7,1:2]=c(1,2)
x[8,1:4]=c(1,2,3,5)
x[9,1:3]=c(1,2,3)
x[10,1:3]=c(1,3,5)

apr = function(x,t1){
  n <- nrow(x)
  m <- ncol(x)
  s <- c()
  p <- c()
  for(i in 1:max(x)){
    if(length(which(x==i))/n>t1){
      s <- c(s,i)
      p <- c(p,length(which(x==i))/n)
    }
  }
  s = s[order(-p)]
  p = p[order(-p)]
  x1 <- matrix(rep(0,n*length(s)),nrow=n)
  for(j in 1:length(s)){
    for(i in 1:n){
     if(s[j] %in% x[i,]){
       x1[i,j] = 1
     } 
    }
  }
  s <- list(s)
  p <- list(p)
  s1 <- list(c(1:length(s[[1]])))
  for(q in 2:m){
    if(length(s[[q-1]])>=3){
      s <- c(s,list(1))
      p <- c(p,list(1))
      s1 <- c(s1,list(1))
      if(q > 2){
        for(i in 2:(length(s[[q-1]])-1)){
          for(j in (i+1):length(s[[q-1]])){
            if(identical(s1[[q-1]][[i]][1:(q-2)], s1[[q-1]][[j]][1:(q-2)])){
              a = 0
              for(i1 in 1:n){
                if(identical(x1[i1,c(s1[[q-1]][[i]],s1[[q-1]][[j]][q-1])],c(rep(1,q)))){
                  a = a+1
                }
              }
              if(a/n>t1){
                s[[q]] <- c(s[[q]],list(c(s[[q-1]][[i]],s[[q-1]][[j]][q-1])))
                p[[q]] <- c(p[[q]],list(a/n))
                s1[[q]] <- c(s1[[q]],list(c(s1[[q-1]][[i]],s1[[q-1]][[j]][q-1])))
              }
            }
          }
        }
      }
      if(q==2){
        for(i in 1:(length(s[[q-1]])-1)){
          for(j in (i+1):length(s[[q-1]])){
            a = 0
            for(i1 in 1:n){
              if(identical(x1[i1,c(i,j)],c(rep(1,q)))){
                 a = a+1
              }
            }
            if(a/n>t1){
              s[[q]] <- c(s[[q]],list(c(s[[q-1]][[i]],s[[q-1]][[j]])))
              p[[q]] <- c(p[[q]],list(a/n))
              s1[[q]] <- c(s1[[q]],list(c(s1[[q-1]][[i]],s1[[q-1]][[j]])))
            }
          }
        }
      }
    }else{
      break
    }
  }
return(list(s,p))
}

apr(x,0.2)


