function g(k,n,A,b)
for i=(k+1):n
    A(i,k)=A(i,k)/A(k,k);
    for j=(k+1):n
        A(i,j)=A(i,j)-A(i,k)*A(k,j);
    end
    b(i,1)=b(i,1)-A(i,k)*b(k,1);
end