%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: x>=0 ∨ y!=0
--the cond is optimal

while (x<y) x’=x+y; y’=-2y;
our method cannot prove non-termination: splitting with y>=0 helps

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X+1=<Y, while(X,Y), X1=X+Y, Y1= -2*Y.
false:- while(X,Y), X>=Y. %return statements

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X+1=<Y, while(X,Y), X1=X+Y, Y1= -2*Y, Y1-X1=< Y-X-1.
while(X1, Y1):- X+1=<Y, while(X,Y), X1=X+Y, Y1= -2*Y, Y1-X1>= Y-X.
false:- while(X,Y), X>=Y. %return statements

*/

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X+1=<Y, while(X,Y), X1=X+Y, Y1= -2*Y.
false:- while(X,Y), X>=Y. %return statements


/*
first cls splitting
init(A,B) :-
   true.
while(A,B) :-
   init(A,B).
while(A,B) :-
   A>=0,
   C+1=<D,
   while(C,D),
   A=C+D,
   B= -2*D.
while(A,B) :-
   A=< -1,
   C+1=<D,
   while(C,D),
   A=C+D,
   B= -2*D.
false :-
   while(A,B),
   A>=B.

 second cls splitting

init(A,B) :-
   true.
while(A,B) :-
   init(A,B).
while(A,B) :-
   A>=0,
   C+1=<D,
   while(C,D),
   A=C+D,
   B= -2*D.
while(A,B) :-
   B>=0,
   A=< -1,
   C+1=<D,
   while(C,D),
   A=C+D,
   B= -2*D.
while(A,B) :-
   B=< -1,
   A=< -1,
   C+1=<D,
   while(C,D),
   A=C+D,
   B= -2*D.
false :-
   while(A,B),
   A>=B.

*/
