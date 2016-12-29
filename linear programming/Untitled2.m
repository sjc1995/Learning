m=3;
n=5;
p=randperm(15);
A=round(p(1)*(rand(m,n)));
b=round(p(2)*(rand(m,1)));
c=round(p(3)*(rand(1,n)));
dodcxf(A,b,c);