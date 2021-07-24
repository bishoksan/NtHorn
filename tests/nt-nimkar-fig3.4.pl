%source: http://discovery.ucl.ac.uk/1469424/1/thesis.pdf


/*
program with repeating cex

int i, k;
 while (i >= 0) {
     if(k>=0){
         i--;
     }else{
         skip;
     }

  }

%our method derived the reqd precond: I>=0, K<0.
*/


init(I, K).
while(I, K):- init(I, K).
while(I1,K):- I>=0, while(I,K), K>=0, I1=I-1.
while(I,K):- I>=0, while(I,K), K=< -1.
false:- while(I,K), I=< -1. %return stmt
