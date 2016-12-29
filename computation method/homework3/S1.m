function [S1]=S1(a,b)
S1=(b-a)/6*(sqrt(a)*log(a)+4*sqrt(a/2+b/2)*log(a/2+b/2)+sqrt(b)*log(b));