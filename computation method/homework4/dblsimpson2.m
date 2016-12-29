function [s3]=dblsimpson2(a,b,m,n,f)
syms x
k=1:n-1;
w=-sqrt(1-x^2):2*sqrt(1-x^2)/(2*n):sqrt(1-x^2);
q=@(x)(2*sqrt(1-x^2)/(6*n)*(f(x,w(1))+4*sum(f(x,w(2*k)))+4*f(x,w(2*n))+2*sum(f(x,w(2*k+1)))+f(x,w(2*n+1))));
s3=simpson(a,b,m,q);
disp(vpa(s3,6));