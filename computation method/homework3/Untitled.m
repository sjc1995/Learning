N=[4, 5];
color=['r','g'];
for i = 1 : length(N);
x=[0:0.01:1];
y = subs(s3(N(i)), x);
plot(x, y,color(i));
hold on;
end;
%根据图像认为应于某一三角函数近似，于是运用以下三角函数拟合
y=1.5+sin((pi/0.8)*(x+0.95));
plot(x,y,'b')
hold on;