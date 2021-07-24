/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
* Adapted from Cairo_true-termination.c
*/

/*
typedef enum {false, true} bool;

//extern int __VERIFIER_nondet_int(void);
int x;
int main()
{
    //int x;
    //x = __VERIFIER_nondet_int();
    if (x > 0) {
        while (x != 0) {
            x = x - 2;
        }
    }
    return 0;
}
*/


/*
our method derives: A=1.
*/
init(X).
if(X):- init(X), X=<0. % Y non-det
if(X):- while(X), X=0.
while(X):- init(X), X>=1.
while(X1):- while(X), X>=1, X1=X-2.
while(X1):- while(X), X=< -1, X1=X-2.
false:- if(X). %return stmt
