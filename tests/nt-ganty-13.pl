%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x≤0∨y<0∨x+y≤0

while (x>0) x’=x+y;

our method derives: -1*x<0,-1*x+ -1*y<0,-1*y<1

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y):- X>=1, while(X,Y), X1=X+Y.
false:- while(X,Y), X=<0. %return statements
