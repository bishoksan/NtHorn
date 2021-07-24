%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x≤0 ∨ z<0 ∨ (z=0 ∧ y<0)∨ x+y≤0 ∨ x+2y+z≤0 ∨ x+3y+3z≤0

while (x>0) x’=x+y; y’=y+z;

our method cannot prove non-termination (non-deterministic value of z, but if we split the loop for the value of z, then it can prove non-termination)

init(X, Y, Z).
while(X, Y, Z):- init(X, Y, Z).
%safe:-init(X, Y, Z).
while(X1, Y1, Z):- X>=1, while(X,Y, Z), X1=X+Y, Y1= Y+Z.
false:- while(X,Y, Z), X=<0. %return statements

spec:- false.
spec:- safe.




init(X, Y, Z).
while(X, Y, Z):- init(X, Y, Z).
%safe:-init(X, Y, Z).
while(X1, Y1, Z):- X>=1, while(X,Y, Z), X1=X+Y, Y1= Y+Z, X1=< -1.
while(X1, Y1, Z):- X>=1, while(X,Y, Z), X1=X+Y, Y1= Y+Z, X1>=0, Y1>=0, Z>=0.
while(X1, Y1, Z):- X>=1, while(X,Y, Z), X1=X+Y, Y1= Y+Z, Y1=< -1.
while(X1, Y1, Z):- X>=1, while(X,Y, Z), X1=X+Y, Y1= Y+Z, Z=< -1.
false:- while(X,Y, Z), X=<0. %return statements

spec:- false.
spec:- safe.


*/

init(X, Y, Z).
while(X, Y, Z):- init(X, Y, Z).
%safe:-init(X, Y, Z).
while(X1, Y1, Z):- X>=1, while(X,Y, Z), X1=X+Y, Y1= Y+Z.
false:- while(X,Y, Z), X=<0. %return statements

