dist = function(a,b){
  if(length(a) == length(b)){
    distance <- sqrt(sum((a-b)^2))
  }else{
    print("wrong")
  }
  return(distance)}

initial = function(A,k){
  n <- nrow(A)
  x <- sample(1:n, 1)
  d1 <- matrix(rep(0, (n+1)*k), ncol = (n+1))
  #zero matrix d1
  d1[1,n+1] <- x
  for(i in 1:(k-1)){
    for(j in 1:n){
      d<-0
      for(a in 1:i){
        d <- d + dist(A[j,],A[d1[a,n+1],])
      }
      d1[i,j] <- d
    }
    d1[i,d1[1:i,n+1]] <- 0
    d1[i+1,n+1] <- which(d1[i,1:n] == max(d1[i,1:n]))[1]
  }
  return(d1[,n+1])
}

km = function(A,k){
  x <- initial(A,k)
  n <- nrow(A)
  center <- rep(0,ncol(A))
  for(i in 1:ncol(A)){
    center[i] <- sum(A[,i])/n
  }
  z <- rep(0,n)
  new <- matrix(rep(0,k*(ncol(A))), ncol = ncol(A))
  for(i in 1:k){
    new[i,] = A[x[i],]
  }
  for(a in 1:800){
    for(i in 1:n){
      tem <- rep(0,k)
      for(j in 1:k){
        tem[j] <- dist(A[i,],new[j,])
      }
      z[i] <- which(tem == min(tem))
    }
    tem1 <- matrix(rep(0,k*(ncol(A))), ncol = ncol(A))
    tem <- matrix(rep(0,k*(ncol(A))), ncol = ncol(A))
    for(i in 1:n){
      tem1[z[i],] = tem1[z[i],] + A[i,]
      tem[z[i],] = tem[z[i],] + 1
    }
    old <- new
    new <- tem1/tem
    if(identical(old, new)){
      break
    }
    W <- 0
    for(i in 1:n){
      W <- W + (dist(A[i,],new[z[i],]))^2
    }
    B <- 0
    for(i in 1:k){
      B <- B + (dist(new[i,],center))^2
    }
    D <- rep(0,k)
    for(i in 1:k){
      for(j in length(which(z == i))){
        for(a in 1:j){
          D[i] <- D[i] + dist(A[which(z == i)[j],],A[which(z == i)[a],])/(2*(length(which(z == i))))
        }
      }
    }
    D1 <- sum(D)
  }
  return(list(new,z,W,B,D1))
}

spectral_cluseter = function(A,k,v){
  n <- nrow(A)
  W <- matrix(rep(0,n*n),ncol = n)
  for( i in 1:n){
    for(j in 1:n){
      W[i,j] = dist(A[i,],A[j,])
    }
  }
  sigma = 1
  if(v == 1){
    epsilon = 2
    for(i in 1:n){
      for(j in 1:n){
        if(W[i,j] <= epsilon){
          W[i,j] = exp(-(W[i,j])^2/(2*sigma^2))
        }else{
          W[i,j] = 0
        }
      }
    }
  }
  if(v == 2){
    q = 5
    for(i in 1:n){
      x = sort(W[i,], index.return = TRUE)
      for(j in 1:n){
        if(is.element(j,x$ix[2:q])){
          W[i,j] = exp(-(W[i,j])^2/(2*sigma^2))
        }else{
          W[i,j]=0
        }
      }
    }
    for(i in 1:n){
      for(j in i:n){
        if(W[i,j]!=0||W[j,i]!=0){
          W[i,j] = max(W[i,j],W[j,i])
          W[j,i] = max(W[i,j],W[j,i])
        }
      }
    }
  }
  if(v == 3){
    for(i in 1:n){
      for(j in 1:n){
        W[i,j] = exp(-(W[i,j])^2/(2*sigma^2))
      }
    }
  }
  D <- matrix(rep(0,n*n),ncol = n)
  for(i in 1:n){
    D[i,i] = sum(W[i,])
  }
  L = D - W
  C = eigen(L)
  y = sort(C$values,index.return = TRUE)
  Z <- matrix(rep(0,k*n),ncol = k)
  for(i in 1:k){
    Z[,i] = C$vectors[,y$ix[i]]
  }
  print(Z)
  return(Z)
}

plt = function(A,z){
  for(i in 1:nrow(A)){
    plot(A[i,1],A[i,2],col = z[i],xlim = c(-10,10),ylim = c(-10,10))
    par(new =T)
  }
}


