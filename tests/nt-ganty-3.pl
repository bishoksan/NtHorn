%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x>n∨x+y≥0

while (x≤N)
if (*) { x’=2*x+y; y’=y+1; } else x’=x+1;

our method cannot prove non-termination (non-deterministic branch)

init(X, Y, N).
while(X, Y, N):- init(X,Y, N).
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1.
while(X1, Y, N):- X=<N, while(X,Y, N), X1=X+1.
false:- while(X,Y, N), X>= N+1. %return statements



init(X, Y, N).
while(X, Y, N):- init(X,Y, N).
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1, X1>=0, Y1>=0,N>=0.
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1, X1=< -1.
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1, Y1=< -1.
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1, N=< -1.
while(X1, Y, N):- X=<N, while(X,Y, N), X1=X+1.
false:- while(X,Y, N), X>= N+1. %return statements

init(X, Y, N).
while(X, Y, N):- init(X,Y, N).
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1, X+Y>=0.
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1, X+Y=< -1.
%while(X1, Y, N):- X=<N, while(X,Y, N), X1=X+1.
false:- while(X,Y, N), X>= N+1. %return statements

*/

init(X, Y, N).
while(X, Y, N):- init(X,Y, N).
while(X1, Y1, N):- X=<N, while(X,Y, N), X1=2*X+Y, Y1= Y+1.
while(X1, Y, N):- X=<N, while(X,Y, N), X1=X+1.
false:- while(X,Y, N), X>= N+1. %return statements



