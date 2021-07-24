%source: http://discovery.ucl.ac.uk/1469424/1/thesis.pdf


/*
Aperiodic  non-termination-Nested loops
Our tool infers non-termination precondition  k>=0.

int j, k;
 while (k >= 0) {
     k++; j=k;
     while(j>=1){
         j--;
      }
  }


*/


init(J, K).
while1(J, K):- init(J, K).
while2(J1,K1):- while1(J, K), K>=0, K1=K+1, J1=K.
while2(J1,K):- J>=1, J1=J-1, while2(J,K).
while1(J, K):- while2(J, K), J=<0.
false:- while1(J,K), K=< -1. %return stmt
