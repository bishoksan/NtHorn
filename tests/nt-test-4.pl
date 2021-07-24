init(A).
while(A):- init(A).
while(A1):- A>=0, A1>=0, while(A).
false:- A=< -1, while(A).

