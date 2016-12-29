function [output]=sas(a,b,d)
f=S1(a,b);
f1=S1(a,(a+b)/2);
f2=S1((a+b)/2,b);
if abs(f-f1-f2)<15*d*(b-a)
    output=f1+f2;
else
    output=sas(a,(a+b)/2,d)+sas((a+b)/2,b,d);
    
end