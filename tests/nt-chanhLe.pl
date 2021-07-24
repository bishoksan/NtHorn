%taken from Termination and non-termination specification inference: PLDI15

init(X,Y).
safe:- init(X,Y).
foo(X, Y):-  init(X,Y).
foo(X1,Y):- X>=0, X1=X+Y, foo(X,Y).
false:-  X=< -1, foo(X,Y). %this clause is needed to show exit cond. of the loop

spec:- false.
spec:- safe.



