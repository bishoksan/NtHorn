%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: y ≤ −10 ∨ x ≥ 10 --is optimal

while (x<10) x’=-y; y’=y+1;

our method derives: 1*x<10,-1*y<10

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X=<9, while(X,Y), X1= -Y, Y1=Y+1.
false:- while(X,Y), X>=10. %return statements
