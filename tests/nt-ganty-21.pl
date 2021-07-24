%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: 4x + y ≤ 0 ∨
(x−4x ≥ 0∧8x−15y ≥ 1)

while (4x+y>0) x’=-2x+4y; y’=4x;

our method cannot prove:

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X.
false:- while(X,Y), 4*X+Y=<0. %return statements

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X, X1>=0, Y1>=0.
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X, X1=< -1.
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X, Y1=< -1.
false:- while(X,Y), 4*X+Y=<0. %return statements


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X, X1>=0, Y1>=0.
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X, X1=< -1, Y1>=0.
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X, Y1=< -1, X1>=0.
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X, Y1=< -1, X1=< -1.
false:- while(X,Y), 4*X+Y=<0. %return statements

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X.
false:- while(X,Y), 4*X+Y=<0. %return statements
