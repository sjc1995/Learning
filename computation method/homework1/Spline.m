function [S]=Spline(x);
syms x;
h=[0.2,0.2,0.2,0.2];
c=[2,2,2,2,2];
a(4)=0;
b(1)=0;
for i=1:3
    a(i)=h(i)/(h(i)+h(i+1));
    b(i+1)=h(i+1)/(h(i)+h(i+1));
end
A=diag(c,0)+diag(a,-1)+diag(b,1);
t=[0.2,0.4,0.6,0.8,1.0];
y=[0.98,0.92,0.81,0.64,0.38];
n=length(t);
z=zeros(n,n);
z(:,1)=y;
i=2;
f=y(1);
for i=2:n
    for j=i:n
    z(j,i)=(z(j,i-1)-z(j-1,i-1))/(t(j)-t(j-i+1));
    end
end
d(1,1)=0;
d(n,1)=0;
for i=2:(n-1)
    d(i,1)=6*z(i+1,3);
end
M=A^-1*d;
k=[0,1,10,11];
x0=0.2+0.08*k;
    for i=1:4
        if x>=t(i)
            if x<=t(i+1)
                S=(M(i,1)*((t(i+1)-x)^3)/(6*h(i)))+(M(i+1,1)*((x-t(i))^3)/(6*h(i)))+((y(i)-(M(i,1)*h(i)^2)/6)*(t(i+1)-x)/h(i))+((y(i+1)-(M(i+1,1)*h(i)^2)/6)*(x-t(i))/h(i));
            end
        end
    end
