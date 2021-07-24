%source: http://discovery.ucl.ac.uk/1469424/1/thesis.pdf


/*
nested loop, inner loop non-terminating: precond derived by our tool I=10.
int i;
 if (i == 10) {
 while (i > 0) {
     i := i − 1;
     while (i == 0)
         skip;
 }
 }


*/


init(X).
if_exit(X):- init(X), X>=11.
if_exit(X):- init(X), X=<9.
while1(X):- init(X), X=10.
while2(X):- while1(X), X>=1, X1=X-1.
while2(X):- X=0, while2(X).
while1(X):- X>=1, while2(X).
while1(X):- X=< -1, while2(X).
if_exit(X):- X=< 0, while1(X).
false:- if_exit(X). %return statements
