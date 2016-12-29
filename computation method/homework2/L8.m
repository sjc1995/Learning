function [L8]=L8(x);
t=[0,1,4,9,16,25,36,49,64];
y=[0,1,2,3,4,5,6,7,8];
w=ones(1,9);
h=ones(1,9);
for i=1:9
    for j=1:9
        if j~=i
            h(i)=h(i)*(t(i)-t(j));
            w(i)=w(i)*(x-(t(j)));
        end
    end
end
L8=0;
for i=1:9
    L8=L8+y(i)*w(i)/h(i);
end


    


