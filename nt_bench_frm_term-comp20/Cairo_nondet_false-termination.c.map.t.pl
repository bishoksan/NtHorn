/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
* Adapted from Cairo_true-termination.c
*/

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x;
int main()
{
    //int x;
    //x = __VERIFIER_nondet_int();
    if (x > __VERIFIER_nondet_int()) {
        while (x != 0) {
            x = x - 1;
        }
    }
    return 0;
}
*/

/*
our method derives: A<0.
*/
/*
init(X).
if(X):- init(X), Y>=X. % Y non-det
if(X):- while(X), X=0.
while(X):- init(X), X>=Y+1.
while(X1):- while(X), X>=1, X1=X-1.
while(X1):- while(X), X=< -1, X1=X-1.
false:- if(X). %return stmt
*/

init(X).
if(X, Y):- init(X), Y>=X. % Y non-det
if(X,Y):- while(X, Y), X=0.
while(X,Y):- init(X), X>=Y+1.
while(X1, Y):- while(X, Y), X>=1, X1=X-1.
while(X1, Y):- while(X, Y), X=< -1, X1=X-1.
false:- if(X, Y). %return stmt
