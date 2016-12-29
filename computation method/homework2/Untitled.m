for x0=0:0.01:64
    i=1;
    x=x0(i);
    i=i+1;
    y1=L8(x);
    y2=s1(x);
    y3=sqrt(x);
    plot(x,y1,'r');
    hold on
    plot(x,y2,'g');
    hold on
    plot(x,y3,'b');
    hold on
end
%根据图中的可知，红线的拟合明显不如绿线的拟合，所以三次样条差值法拟合比拉格朗日插值点拟合更精准%