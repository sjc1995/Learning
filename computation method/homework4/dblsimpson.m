function [s2]=dblsimpson(a,b,c,d,m,n,f)
syms x
k=1:n-1;
w=c:(d-c)/(2*n):d;
q=@(x)((d-c)/(6*n)*(f(x,w(1))+4*sum(f(x,w(2*k)))+4*f(x,w(2*n))+2*sum(f(x,w(2*k+1)))+f(x,w(2*n+1))));
s2=simpson(a,b,m,q);