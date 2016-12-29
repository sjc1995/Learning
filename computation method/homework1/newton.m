function [f]=newton(x);
syms x;
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
    h=z(i,i);
    for a=1:(i-1)
        h=h*(x-t(a));
    end
    f=f+h;
end
k=[0,1,10,11];
x0=0.2+0.08*k;
for i=1:4
    x=x0(i);
y0(i)=subs(f,x);
end
disp(z)
disp(y0)
disp(collect(f))
plot(x0,y0)


