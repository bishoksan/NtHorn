%taken from Thttp://www-verimag.imag.fr/~bakhirki/pdf/tacas16.pdf

/*
while(0 <= x<=100){
    if(*){
        x=x-1;
    }else{
        x=x-1;
    }
}
*/

init(X).
while(X):- init(X).
if_exit(X1):- while(X), 0 =< X, X=< 100, X1=X-1.
if_exit(X1):- while(X), 0 =< X, X=< 100, X1=X+1.
while(X):- if_exit(X).
false:- while(X), X=< -1. %return stmt
false:- while(X), X>= 101. %return stmt

spec:- false.
spec:- safe.



