%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: n ≤ 200 ∨ y ≥ 9 ∨
(x < n ∧ y ≥ 1) ∨
(x<n ∧ x≥200 ∧ x+y≥200)


@requires n>200 and y<9
while (1){
if (x<n) {
    x’=x+y;
    if (x’≥200) break;
}
}

our method cannot prove non-termination (non-deterministic branch)

%chc encoding not right


*/


init(X, Y, N):- N>=201, Y=<8.
while(X, Y, N):- init(X,Y, N).
if2_entry(X1,Y,N):- while(X, Y, N), X+1=<N, X1=X+Y.

if1_exit(X,Y,N):- while(X, Y, N), X>=N.
if1_exit(X,Y,N):- if2_entry(X,Y,N), X=< 199.
while(X, Y, N):- if1_exit(X,Y,N).
false:- if2_entry(X,Y,N), X>= 200. %return statements
