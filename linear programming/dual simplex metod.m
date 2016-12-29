function dodcxf=dodcxf(A,b,c)
[m,n]=size(A);
B=zeros(m,m);
i=1:m;
j=1:n;
for h=1:(n^n)
    p=randperm(n);
    B(:,i)=A(:,p(i));
    b1=B\b;
    cB(i)=c(p(i));
    z=cB*b1;
    r(j)=c(j)-cB*(B^(-1))*A(:,j);
    if length(find(r>=0))==n
        break
    end
end
for h=1:n^n
    k=find(b1==min(b1));
    if b1(k)>=0
        y=(B')\(cB');
        disp('最优解为');
        disp(b1);
        disp('最优值');
        disp(z);
        break
    else
        y1(:,j)=B\A(:,j);
        if length(find(y1(k,j)>=0))==n
            disp('对偶规划无上界');
            break
        else
            t1=find(y1(k,j)<0);
            t=find(r(t1)./(-y1(k,t1))==min(r(t1)./(-y1(k,t1))));
            t=t1(t);
            p(k)=t;
            B(:,i)=A(:,p(i));
            b1=B\b;
            cB(i)=c(p(i));
            z=cB*b1;
            r(j)=c(j)-cB*(B^(-1))*A(:,j);
        end
    end
end