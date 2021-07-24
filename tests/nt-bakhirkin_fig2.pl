%taken from Thttp://www-verimag.imag.fr/~bakhirki/pdf/tacas16.pdf

/*
while(0 <= x<=100){
    y=*;
    x=x+y;
}
*/

init(X, Y).
while(X,Y):- init(X, Y).
while(X1, Y1):- while(X, Y), 0 =< X, X=< 100, X1=X+Y.
false:- while(X, Y), X=< -1. %return stmt
false:- while(X,Y), X>= 101. %return stmt

spec:- false.
spec:- safe.



