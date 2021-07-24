%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x≥0∨x+y≥0∨
x + 2y ≥ 1 ∨ x + 3y ≥ 3

while (x<0) x’=x+y; y’=y-1;

our method derives: x<0,x+y<0,y<2

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X=< -1, while(X,Y), X1=X+Y, Y1=Y-1.

false:- while(X,Y), X>=0. %return statements
