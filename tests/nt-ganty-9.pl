%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x≥0∨ y!=0
--the cond is optimal

while (x<y) x’=x+y; 2y’=y;
our method derives: x-y<0,2*x+y<0,3*y<4 \/
x-y<0,2*x+y<0,-2*x-y<2

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X+1=<Y, while(X,Y), X1=X+Y, Y= 2*Y1.
false:- while(X,Y), X>=Y. %return statements
