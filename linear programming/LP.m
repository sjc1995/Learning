function [L]=LP(c,b,A)
[m,n]=size(A);
for i=1:2^n
    p=randperm(n);
    B=zeros(m,m);
    for j=1:m
        B(:,j)=A(:,p(j));
    end
    if det(B)~=0
        a=B\b;
        if length(find(a>=0))==m
            break
        end
    end
end
L=dcxf(c,b,A,B,p,a,m,n);