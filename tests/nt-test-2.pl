init(A).
while(A):-init(A).
while(A1):- A>=1, A1=A-1, while(A).
while(A1):- A=0,  A1=A+1, while(A).
false:- A=< -1, while(A).
%while(A):-  A1=A-1, while(A).

spec:- false.
spec:- safe.
