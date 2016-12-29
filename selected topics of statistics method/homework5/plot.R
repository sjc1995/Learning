source('ridge_regression.R')
source('best_subsets.R')
source('pcr.R')
source('PLS.R')
source('LASSO.R')
P = read.csv("C:\\Users\\Oregi\\Downloads\\test2.csv")
P = as.matrix(P)
colnames(P) = NULL
P <- P[,2:ncol(P)]
X <- P[,1:(ncol(P)-1)]
Y <- P[,ncol(P)]

beta <- lq(X,Y)

beta1 <- best_subset(X, Y)
plot(c(0,beta1[1],beta[1]),c(0,beta1[2],beta[2]),type='l',xlim=c(-0.5,6.5),ylim=c(-1.5,3.2),xlab = 'beta1',ylab='beta2')
par(new=T)

arrows(0,-1,0,10,lty=2)
arrows(-1,0,10,0,lty=2)
par(new=T)

beta2 <- c()
for(i in seq(0,1,0.1)){
  beta2 <- c(beta2,ridge_regression(X,Y,i))
}
beta2 <- matrix(beta2,nrow=2)
plot(c(beta2[1,],0),c(beta2[2,],0),type='l',xlim=c(-0.5,6.5),ylim=c(-1.5,3.2),xlab = 'beta1',ylab='beta2')
par(new=T)

beta3 <- c()
for(i in 1:2){
  beta3<-c(beta3,pcr(X,Y,i))
}
beta3 <- matrix(beta3,nrow=2)
plot(c(0,beta3[1,]),c(0,beta3[2,]),type='l',xlim=c(-0.5,6.5),ylim=c(-1.5,3.2),xlab = 'beta1',ylab='beta2')
par(new=T)

beta4 <- c()
for(i in 1:2){
  beta4<-c(beta4,PLS(X,Y,i))
}
beta4 <- matrix(beta4,nrow=2)
plot(c(0,beta4[1,1],beta[1]),c(0,beta4[2,1],beta[2]),type='l',xlim=c(-0.5,6.5),ylim=c(-1.5,3.2),xlab = 'beta1',ylab='beta2')
par(new=T)


beta5 <- LASSO(X,Y)
plot(c(beta5[1,]),c(beta5[2,]),type='l',xlim=c(-0.5,6.5),ylim=c(-1.5,3.2),xlab = 'beta1',ylab='beta2')
par(new=T)

points(beta[1],beta[2],pch=20)
points(beta1[1],beta1[2],pch=20)
points(beta3[1,1],beta3[2,1],pch=20)
points(beta4[1,1],beta4[2,1],pch=20)
points(2.1,0,pch=20)

text(5,2,"Least Squares",cex=0.5)
text(3.8,0.4,"Best Subsets",cex=0.5)
text(2.2,1,"Ridge",cex=0.5)
text(3,-0.3,"PLS",cex=0.5)
text(1,-1.2,"PCR",cex=0.5)
text(2.5,0.2,"LASSO",cex=0.5)