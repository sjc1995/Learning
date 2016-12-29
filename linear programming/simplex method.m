function [output]=dcxf(c,b,A,B,p,a,m,n)
cB=zeros(1,m);
for i=1:m
    cB(1,i)=c(p(i));
end
z=cB*a;
r=c-cB*((B^(-1))*A);
min_r=r(1);
t=1;
for i=2:n
    if r(i)<min_r
        min_r=r(i);
        t=i;
    else
    end
end
x=zeros(n,1);
for i=1:m
    x(p(i),1)=a(i,1);
end
if min_r>=0 && length(find(a>=0))==m
    disp('x=');
    disp(x);
    disp('LP=');
    disp(z);
else
    y=B\A(:,t);
    max_y=y(1,1);
    for i=1:m-1
        if y(i+1)>max_y;
            max_y=y(i+1,1);
        end
    end
    if max_y<=0
        disp('LPÎÞÏÂ½ç')
    else
        w=a./y;
        f=find(w>=0);
        min_w=w(f(1));
        q=1;
        if length(f)>1
            for i=1:length(f)-1
                if w(f(i+1))<min_w && w(f(i+1))>0
                    min_w=w(f(i+1));
                    q=i+1;
                end
            end
        end
        for i=1:m
            x(p(i))=x(p(i))-min_w*y(i);
        end
        x(t)=min_w;
        p(q)=t;
        for j=1:m
            B(:,j)=A(:,p(j));
            a=B\b;
        end
    end
    output=dcxf(c,b,A,B,p,a,m,n);
end