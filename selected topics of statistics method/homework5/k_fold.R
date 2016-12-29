source('ridge_regression.R')
source('best_subsets.R')
source('pcr.R')
source('PLS.R')
source('LASSO.R')
source('LARS.R')
P = read.csv("C:\\Users\\Oregi\\Downloads\\test3.csv")
P = as.matrix(P)
colnames(P) = NULL
P <- P[,2:ncol(P)]
X <- P[,1:(ncol(P)-1)]
Y <- P[,ncol(P)]

K_FOLD = function(x,y,k){
  m <- ncol(x)
  n <- nrow(x)
  x <- normalize(x)
  y <- as.matrix(centerlize(y))
  a <- sample(1:n,n)
  q <- floor(n/k)
  MSE <- rep(0,k)
  beta <- best_subset(x,y)
  print(beta)
  for(i in 1:k){
    x_train <- x[setdiff(a,a[(q*(i-1)+1):(q*i)]),]
    y_train <- y[setdiff(a,a[(q*(i-1)+1):(q*i)])]
    x_test <- x[a[(q*(i-1)+1):(q*i)],]
    y_test <- y[a[(q*(i-1)+1):(q*i)]]
    beta <- best_subset(x_train,y_train)
    MSE[i] <- mean((y_test-x_test%*%beta)^2)
  }
  return(mean(MSE))
}

K_FOLD(X,Y,10)