function [s]=simpson(a,b,n,f)
k=1:n-1;
t=a:(b-a)/(2*n):b;
p=subs(f,t);
s=(b-a)/(6*n)*(2*sum(p(2*k+1))+4*(sum(p(2*k))+p(2*n))+p(1)+p(2*n+1));
