l=dblsimpson(0,1,0,1,4,4,@(x,y)(exp(-x*y)));
disp(vpa(l,6));