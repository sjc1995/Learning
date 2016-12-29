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
    d1[i+1,n+1] <- which(d1[i,1:n] == max(d1[i,1:n]))
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

plt = function(A,z){
  plot(c(-10,-10,10,10),c(-10,10,-10,10))
  for(i in 1:nrow(A)){
    points(A[i,3],A[i,4],col = z[i])
  }
}

CH = function(A){
  n <- nrow(A)
  C <- rep(0,9)
  for(k in 2:10){
    a <- km(A,k)
    new <-a[[1]]
    z <- a[[2]]
    W <- a[[3]]
    B <- a[[4]]
    C[k-1] <- B*(n-k)/(W*(k-1))
  }
  for(k in 1:8){
    C[k] <- C[k+1]-C[k]
  }
  k <- which(C[1:8] == max(C[1:8]))+2
  z <- km(A,k)[[2]]
  print(k)
  print(z)
  
}

H = function(A){
  n <- nrow(A)
  C <- rep(0,9)
  for(k in 2:10){
    a <- km(A,k)
    b <- km(A,(k+1))
    W1 <- a[[3]]
    W2 <- b[[3]]
    C[k-1] <- (W1/W2-1)*(n-k-1)
  }
  print(C)
  plot(2:10, C , type = 'b')
  for(k in 1:9){
    if(C[k]<30){
      break
    }
  }
  k = k+1
  print(k)
  z <- km(A,k)[[2]]
#  plt(A,z)
}

GAP = function(A){
  W <- rep(0,9)
  W1 <- rep(0,9)
  G <- rep(0,9)
  for(k in 2:10){
    a <- km(A,k)
    W[k-1] <- a[[5]]
    B <- matrix(rep(0,(ncol(A)*nrow(A))), ncol = ncol(A))
    for(i in 1:ncol(A)){
      B[,i] <- runif(nrow(A),-10,10)
    }
    b <- km(B,k)
    W1[k-1] <- b[[5]]
    G[k-1]= log(W1[k-1])-log(W[k-1])
  }
  plot(2:10,G ,type = 'b')
}


P = read.csv("C:\\Users\\Oregi\\Downloads\\Data1.csv")
P = as.matrix(P)
colnames(P) = NULL
P <- P[,2:ncol(P)]
GAP(P)
