function [T]=T(n);
x=eps : 1/n :1;
y=sqrt(x).*log(x);
T = 1/(2*n)*(2*sum(y));