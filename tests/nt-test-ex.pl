/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
*/

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x;
int y;
int main()
{
    if(y>=100) y = y-100;
    else y= 100-y;
    if (y!=0 ) {
        while (x > 0) {
            x = x + x + y;
            y = y - 1;
        }
    }
    return 0;
}
*/

%manual encoding



init(X,Y).
if1_exit(X,Y1):- Y>=100, Y1= Y-100, init(X,Y).
if1_exit(X,Y1):- Y=<99, Y1= 100-Y, init(X,Y).
%safe:- Y>=1,if1_exit(X,Y).
%safe:- Y=< -1,if1_exit(X,Y).
while(X,Y):- Y>=1,if1_exit(X,Y).
while(X,Y):- Y=< -1,if1_exit(X,Y).
while(X1,Y1):- X>=1, X1=2*X+Y, Y1=Y-1 , while(X,Y).
%while(X1,Y1):- X>=1, X1=2*X+Y, Y1=Y-1, X1=<X-1 , while(X,Y).
%while(X1,Y1):- X>=1, X1=2*X+Y, Y1=Y-1, X1>=X, while(X,Y).
false:- while(X,Y), X=<0.
false:- if1_exit(X,Y), Y=0.

%spec:- false.
%spec:- safe.
