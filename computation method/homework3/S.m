function [S]=S(n)
k=1:n;
x=eps:1/(2*n):1;
y=sqrt(x).*log(x);
S=1/(6*n)*(2*sum(y(2*k+1))+4*sum(y(2*k)));