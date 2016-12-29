Hie = function(A,z){
  n <- nrow(A)
  B <- matrix(rep(0,(n*n)),ncol=n)
  Result <- matrix(rep(0,(2*n-2)),ncol=2)
  for(i in 1:n){
    for(j in 1:n){
      B[i,j] <- dist(A[i,],A[j,])
    }
  }
  for(s in 1:(n-1)){
    c <- which(B == min(B[which(B != 0)]))[1]
    if(c%%n!=0){
      x1 <- c%%n
      x2 <- c%/%n + 1
    }
    if(c%%n==0){
      x1 <- n
      x2 <- c%/%n
    }
    Result[s,1] <- x1
    Result[s,2] <- x2
    if(z == 1){
      m = min(x2,x1)
      M = max(x2,x1)
      print(m)
      for(i in 1:n){
        B[m,i] <- min(B[m,i],B[M,i])
        B[i,m] <- min(B[i,m],B[i,M])
        B[M,i] <- 0
        B[i,M] <- 0
      }
    }
    if(z == 2){
      m = min(x2,x1)
      M = max(x2,x1)
      print(m)
      for(i in 1:n){
        B[m,i] <- max(B[m,i],B[M,i])
        B[i,m] <- max(B[i,m],B[i,M])
        B[i,i] <- 0
        B[M,i] <- 0
        B[i,M] <- 0
      }
    }
    if(z == 3){
      m = min(x2,x1)
      M = max(x2,x1)
      print(m)
      for(i in 1:n){
        B[m,i] <- mean(B[m,i],B[M,i])
        B[i,m] <- mean(B[i,m],B[i,M])
        B[i,i] <- 0
        B[M,i] <- 0
        B[i,M] <- 0
      }
    }
  }
  return(Result)
}

P = read.csv("C:\\Users\\Oregi\\Downloads\\Data2.csv")
P = as.matrix(P)
colnames(P) = NULL
P <- P[,2:ncol(P)]
