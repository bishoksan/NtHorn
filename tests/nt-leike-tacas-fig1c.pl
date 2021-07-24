%source: from the paper "Geometric Nontermination Arguments",
%by Jan Leike and Matthias Heizmann (TACAS'18')



/*

b := 1 ;
while ( a+b >= 4 ) :
a := 3∗a + b ;
b := 2∗b ;

*/


init(A, B).
while(A, B):- B=1, init(A, B).
while(A1, B1):- A+B>=4, while(A,B), A1=3*X+B, B1=2*B.
false:- while(A,B), A+B=<3. %return statements
