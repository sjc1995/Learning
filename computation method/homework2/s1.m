function [s1]=s1(x);
t=[0,1,4,9,16,25,36,49,64];
y=[0,1,2,3,4,5,6,7,8];
a(1)=1;
b(8)=1;
for i=1:7
    a(i+1)=(t(i+2)-t(i+1))/((t(i+1)-t(i))+(t(i+2)-t(i+1)));
    b(i)=(t(i+1)-t(i))/((t(i+1)-t(i))+(t(i+2)-t(i+1)));
end
c=2*ones(1,9);
A=diag(c,0)+diag(a,1)+diag(b,-1);
n=length(t);
z=zeros(n,n);
z(:,1)=y;
for i=2:n
    for j=i:n
    z(j,i)=(z(j,i-1)-z(j-1,i-1))/(t(j)-t(j-i+1));
    end
end
d(1,1)=6*(z(2,2)-0);
d(n,1)=6*((1/128)-z(n,2))/15;
for i=2:n-1
    d(i,1)=6*z(i+1,3);
end
M=(A^-1)*d;
s1=0;
for i=1:8
    if x>=t(i)
        if x<t(i+1)
            s1=s1+(M(i,1)*((t(i+1)-x)^3)/(6*(t(i+1)-t(i))))+(M(i+1,1)*((x-t(i))^3)/(6*(t(i+1)-t(i))))+((y(i)-(M(i,1)*(t(i+1)-t(i))^2)/6)*(t(i+1)-x)/(t(i+1)-t(i)))+((y(i+1)-(M(i+1,1)*(t(i+1)-t(i))^2)/6)*(x-t(i))/(t(i+1)-t(i)));
        end
    end
end
