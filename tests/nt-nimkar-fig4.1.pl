%source: http://discovery.ucl.ac.uk/1469424/1/thesis.pdf


/*
non-termination with max-smt

int i; int j=-1;
 while (i > 0 and j!=0) {
     i+=j;
     j+=2;

  }

%our method derived the reqd precond: I>=2, J= -1.
*/


init(I, J):- J= -1.
while(I, J):- init(I, J).
while(I1, J1):- I>= 1, J>=1, while(I, J), I1= I+J, J1=J+2.
while(I1, J1):- I>= 1, J=< -1, while(I, J), I1= I+J, J1=J+2.
false:- while(I,J), I=<0. %return stmt
false:- while(I,J), J=0. %return stmt
