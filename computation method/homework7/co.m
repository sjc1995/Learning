function co=co(n);
A=zeros(n,n);
for i=1:n
    j=1:n;
    A(i,j)=1./(i+j-1);
end
co=cond(A,inf);
