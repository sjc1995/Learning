function f=f(n)
format long
H=zeros(n,n);
x=ones(n,1);
for i=1:n
    j=1:n;
    H(i,j)=1./(i+j-1);
end
b=H*x;
for k=1:(n-1)
    i=k:n;
    ik=find(abs(H(i,k))==max(abs(H(i,k))))+k-1;
     if max(abs(H(:,k)))==0
        break
    else
        if ik==k
            for i=(k+1):n
                H(i,k)=H(i,k)/H(k,k);
                j=(k+1):n;
                H(i,j)=H(i,j)-H(i,k)*H(k,j);
                b(i,1)=b(i,1)-H(i,k)*b(k,1);
            end
        else
            j=k:n;
            c=H(ik,j);
            H(ik,j)=H(k,j);
            H(k,j)=c;
            b1=b(ik,1);
            b(ik,1)=b(k,1);
            b(k,1)=b1;
            clear c b1
            for i=(k+1):n
                H(i,k)=H(i,k)/H(k,k);
                for j=(k+1):n
                    H(i,j)=H(i,j)-H(i,k)*H(k,j);
                end
                b(i,1)=b(i,1)-H(i,k)*b(k,1);
            end
        end
    end
end
if H(n,n)==0
    return
else
    b(n,1)=b(n,1)/H(n,n);
    for i=(n-1):(-1):1
        j=(i+1):n;
        b(i,1)=(b(i,1)-sum(H(i,j)*b(j,1)))/H(i,i);
    end
end
p=H*x;
r=p-H*b;
ox=b-x;
disp('r=');
disp(r);
disp('¦Äx=');
disp(ox);
disp('x¡¯=');
disp(b);