/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
* Adapted from the example 2Nested_true-termination.c
*
* This program does not terminate when x >= 0 & y >= 0
*/

/*
typedef enum {false, true} bool;

//extern int __VERIFIER_nondet_int(void);
int x;
int y;

int main()
{
    
    //x = __VERIFIER_nondet_int();
    //y = __VERIFIER_nondet_int();
    while (x >= 0) {
        x = x + y;
        y = y + 1;
    }
    return 0;
}
*/

%we derive:  X>=0,X+Y>=0, Y>= -1.
init(X,Y).
while(X,Y):- init(X,Y).
while(X1,Y1):- while(X,Y), X>=0, X1=X+Y, Y1=Y+1.
false:- while(X,Y), X=< -1. %return stmt

