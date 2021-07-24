%init(X,Y):- X = 100, Y = 0.
init(X,Y):- X = 2, Y = 0.
while(X, Y):- init(X,Y).
while(X1, Y):- X>=2, X1=X-1, while(X,Y).
while_exit(X,Y):- X=<1, while(X,Y).
l(X,Y):- while_exit(X,Y).
l(X1,Y1):- X1 = X+Y, Y1 = Y+1, l(X,Y).
false :-  Y > X, l(X,Y).


spec:- false.
spec:- safe.
