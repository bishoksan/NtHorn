%taken from http://www-verimag.imag.fr/~bakhirki/pdf/tacas16.pdf

/*
while(0 <= x){
    
    x=x+y;
}
*/

init(X, Y).
while(X,Y):- init(X, Y).
%safe:- init(X, Y).
while(X1, Y):- while(X, Y), 0 =< X, X1=X+Y.
false:- while(X, Y), X=< -1. %return stmt

spec:- false.
spec:- safe.



