init(X,Y).
safe:- init(X,Y), X< -2147483647.
safe:- init(X,Y), Y< -2147483647.
while_entry(X,Y):- init(X,Y),X>= -2147483647, Y>= -2147483647.
while(X_old, X,Y):- while_entry(X,Y).
while(X_old1,X1,Y1):- X_old= X, X1= -Y, Y1=X_old, while(X_old, X,Y).
false:- 1=0, while(X_old,X,Y).
