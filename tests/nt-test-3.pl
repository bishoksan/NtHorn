init(A).
while(A, B):-B=10, init(A).
while(A1, B1):- A>=1, B1=B-1, while(A,B).
false:- A=< 0, while(A,B).

