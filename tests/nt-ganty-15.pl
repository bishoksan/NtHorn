%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x≥0∨x+z≥0

 while (x<0) x’=x+z; y’=y+1; z’=-2y

our method cannot prove: splitting by Z does not help

init(X, Y, Z).
while(X, Y, Z):- init(X, Y, Z).
while(X1, Y1, Z1):- X=< -1, while(X,Y, Z), X1=X+Z, Y1=Y+1, Z1= -2*Y.
false:- while(X,Y, Z), X>=0. %return statements

*/



init(X, Y, Z).
while(X, Y, Z):- init(X, Y, Z).
while(X1, Y1, Z1):- X=< -1, while(X,Y, Z), X1=X+Z, Y1=Y+1, Z1= -2*Y.
false:- while(X,Y, Z), X>=0. %return statements
