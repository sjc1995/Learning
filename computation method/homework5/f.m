function f=f(A,b)
n=length(b');
con=cond(A);
d=det(A);
h=1:n;
for k=1:(n-1)
    i=k:n;
    ik=find(abs(A(i,k))==max(abs(A(i,k))))+k-1;
     if max(abs(A(:,k)))==0
        d=0;
        break
    else
        if ik==k
            for i=(k+1):n
                A(i,k)=A(i,k)/A(k,k);
                for j=(k+1):n
                    A(i,j)=A(i,j)-A(i,k)*A(k,j);
                end
                b(i,1)=b(i,1)-A(i,k)*b(k,1);
            end
            d=A(k,k)*d;
        else
            j=k:n;
            c=A(ik,j);
            A(ik,j)=A(k,j);
            A(k,j)=c;
            clear c
            for i=(k+1):n
                A(i,k)=A(i,k)/A(k,k);
                for j=(k+1):n
                    A(i,j)=A(i,j)-A(i,k)*A(k,j);
                end
                b(i,1)=b(i,1)-A(i,k)*b(k,1);
            end
            d=-1*d;
        end
    end
end
if A(n,n)==0
    d=0;
else
    b(n,1)=b(n,1)/A(n,n);
    for i=(n-1):(-1):1
        j=(i+1):n;
        b(i,1)=(b(i,1)-sum(A(i,j)*b(j,1)))/A(i,i);
    end
end
d=A(n,n)*d;
disp('A=');
disp(A);
disp('det=');
disp(d);
disp('x=');
disp(b);
disp('cond=');
disp(con);


