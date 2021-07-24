/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
* Adapted from Mysore_true-termination.c
*/
/*
typedef enum {false, true} bool;

//extern int __VERIFIER_nondet_int(void);

int x;
int c;
int main()
{
    
    //x = __VERIFIER_nondet_int();
    //c = __VERIFIER_nondet_int();
    if (c < 0) {
        while (x + c >= 0) {
            x = x - c;
            c = c - 1;
        }
    }
    return 0;
}
*/

%our method derives [[-C>=1,-X-C<1],[-C>=1,-X-C<1]]
init(X,C).
while(X,C):- C=< -1,init(X,C).
%safe:- C=< -1,init(X,C).
while(X1,C1):- X+C>=0, X1=X-C, C1=C-1, while(X,C).
false:- while(X,C), X+C=< -1.
false:- init(X,C), C>=0.
spec:- false.
spec:- safe.
