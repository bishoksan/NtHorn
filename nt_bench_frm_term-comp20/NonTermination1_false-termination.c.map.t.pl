/*
* Date: 2014-06-26
* Author: leike@informatik.uni-freiburg.de
*
*/

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x;
int main() {
    
    while (x > 1) {
        x = 2*x;
    }
    return 0;
}
*/


init(X).
while(X):- init(X).
while(X1):- X>=2, X1= 2*X, while(X).
false:- X=< 1, while(X).
