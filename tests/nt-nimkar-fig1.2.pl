%source: http://discovery.ucl.ac.uk/1469424/1/thesis.pdf


/*
nested loop, outer loop non-terminating
Our tools infers i>10 as non-termination condition.

int i,j;
 while (i > 10) {
     i++;
     j=2;
     while (j > 0) {
         j--;
     }
 }


*/


init(I,J).
while1(I,J):- init(I,J).
false:- I=< 10, while1(I,J). %return stmt
while2(I1,J1):- while1(I,J), I>=11, I1=I+1, J1=2.
while2(I,J1):- J>=1, J1=J-1, while2(I,J).
while1(I,J):- J=< 0, while2(I,J).
