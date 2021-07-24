%taken from Thttp://www-verimag.imag.fr/~bakhirki/pdf/tacas16.pdf

/*
if(a<b){
    swap(a,b);
}
while(a!=b){
    t=a-b,
    a=b;
    b=t;
}
*/


init(A, B).
if_exit(A,B):-
    A>=B, init(A,B).
if_exit(B,A):-
    A=< B-1, init(A,B).
while(A,B):- if_exit(A,B).
while(A1,B1):- A=< B-1, while(A,B),  A1=B, B1=A-B.
while(A1,B1):- A-1 >= B, while(A,B),  A1=B, B1=A-B.
false:- while(A, B), A=B. %return stmt

spec:- false.
spec:- safe.


