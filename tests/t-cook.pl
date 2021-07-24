/*
source: Proving Conditional Termination by Cook et al. CAV-08
*/

/*


while(x>=0){
    x=x+y;
}

*/

% original

init(A, B).
loop(A,B):- init(A, B).
loop(A1,B):- A>=0, A1=A+B, loop(A,B).
false:- A=< -1, loop(A,B).


spec:- false.
spec:- safe.



