%source: from the paper "Proving Termination Starting from the End",
%by Pierre Ganty and Samir Genaim



/*
termination precond from the paper: 5y − 4x ≥ 0 ∨
(3x−4y ≥ 0∧16x−21y ≥ 1)
--the cond is optimal

while (4x-5y>0) x’=2x+4y; y’=4x;

our method derives: -4*x+5*y<0,3*x+ -4*y<0

*/


init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- 4*X-5*Y>=1, while(X,Y), X1=2*X+4*Y, Y1= 4*X.
false:- while(X,Y), 4*X-5*Y=<0. %return statements
