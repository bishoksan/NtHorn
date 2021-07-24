%source: http://discovery.ucl.ac.uk/1469424/1/thesis.pdf


/*
Aperiodic  non-termination-single loop
Our tool infers j>=0, i>=j as non-termination precondition.

int i,j, k;
 if (j >= 0) {
     while(i>=j){
         k=i-j;
         if(k>0){ i--;} else{i=2*i+1; j++}
     
      }
  }


*/


init(I,J, K).
while(I,J, K):- init(I,J, K), J>=0.
if2_exit(I1,J,K1):- while(I,J, K), I>=J, K1= I-J, K1>=1, I1= I-1.
if2_exit(I1,J1,K1):- while(I,J, K), I>=J, K1= I-J, K1=< 0, I1= 2*I+1, J1=J+1.
while(I,J, K):- if2_exit(I,J,K).
if1_exit(I,J,K):- I=< J-1, while(I,J, K).
if1_exit(I,J,K):- init(I,J, K), J =< -1.
false:- if1_exit(I,J,K). %return stmt


