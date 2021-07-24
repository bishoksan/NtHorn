%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x ≤ 3 ∨ 10y − 3x!=0--the cond is optimal

while (x>0 and y>0) x’=-2x+10y;

our method  derives: -1*x<0,1*y<1,-1*x+10*y<2

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y):- X>=1, Y>=1, while(X,Y), X1= -2*X+10*Y.
false:- while(X,Y), X=<0. %return statements
false:- while(X,Y), Y=<0. %return statements

