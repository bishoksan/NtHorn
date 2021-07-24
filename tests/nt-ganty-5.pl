%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: (x ≥ 1 ∧ y ≥ 1) ∨ x = y
--the cond is optimal
while (x<>y) if (x>y) x’=x-y; else y’=y-x;

our method derives: x>y, y<1 \/ x<y, x<1

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y):- X>=Y+1, while(X,Y), X1=X-Y.
while(X, Y1):- X+1=<Y, while(X,Y), Y1=Y-X.
false:- while(X,Y), X=Y. %return statements
