function [t1]=t1(n);
A=zeros(n+1,n+1);
for i=1:n
    x=eps:1/(2^i):1;
    y=sqrt(x).*log(x);
    k=1:2^(i-1);
    A(i+1,1)=1/2*A(i,1)+1/(2^i)*(sum(y(2*k)));
end
for i=2:n+1
    for j=i:n+1
        A(j,i)=4^(i-1)/(4^(i-1)-1)*A(j,i-1)-1/(4^(i-1)-1)*A(j-1,i-1);
    end
end
t1=A(n+1,n+1);