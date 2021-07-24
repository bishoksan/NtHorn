%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x≤0 ∨ y!=0
--the cond is optimal

while (x>0) x’=x+y; y’=-2y;
our method cannot prove non-termination: splitting with X1>=0, Y1>=0 helps

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X>=1, while(X,Y), X1=X+Y, Y1= -2*Y.
false:- while(X,Y), X=<0. %return statements

*/

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X>=1, while(X,Y), X1=X+Y, Y1= -2*Y.
false:- while(X,Y), X=<0. %return statements
