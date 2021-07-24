%source: from the paper "confilcit driven conditional termination",
%by Urban et al.



/*
termination precond from the paper: x>=-2
while (x< -2 || x>2) x--;

our method derives: A in [-3,2]

*/


init(X).
while(X):- init(X).
while(X1):- X=< -3, while(X), X1=X-1.
while(X1):- X>= 3, while(X), X1=X-1.

false:- while(X), X>= -2, X=<2. %return statements
