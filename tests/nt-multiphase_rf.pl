/*source: from the paper "Multiphase-Linear Ranking Functions and their Relation to Recurrent Sets"
by Amir, Jesus, Samir
*/

/*
while(a>=0){ap=1-a}
*/

init(A).
wh(A):- init(A).
wh(Ap):- A>=0, wh(A), Ap= 1-A.
false:- A<0, wh(A).



