%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x!=0 ∨ y!=0--the cond is optimal

while (x<5) x’=x-y; y’=x+y;

our method cannot prove: clause splitting is the solution



init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X=<4, while(X,Y), X1=X-Y, Y1= X+Y.
false:- while(X,Y), X>=5. %return statements


splitting using x1>=0,y1>=0 works
*/

/*
init(X, Y).
%safe:-init(X,Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X=<4, while(X,Y), X1=X-Y, Y1= X+Y, X1=< X-1. %terminating
while(X1, Y1):- X=<4, while(X,Y), X1=X-Y, Y1= X+Y, X1>=X. %non-terminating
%while(X1, Y1):- X=<4, while(X,Y), X1=X-Y, Y1= X+Y. %original

false:- while(X,Y), X>=5. %return statements

spec:-false.
spec:-safe.
*/

init(X, Y).
if2(X,Y):- X>=0, init(X,Y).
while(X, Y):-  Y>=0, if2(X, Y).
while(X1, Y1):- X=<4, while(X,Y), X1=X-Y, Y1= X+Y.

if2_exit(X,Y):- Y=<0, if2(X,Y).
if2_exit(X,Y):- while(X,Y), X>=5.


false:- X=< -1, init(X,Y).
false:- Y=<0, if2(X,Y).
false:- while(X,Y), X>=5.
