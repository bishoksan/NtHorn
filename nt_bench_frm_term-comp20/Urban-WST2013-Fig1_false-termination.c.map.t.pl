/*
 * Program from Fig.2 of
 * 2013WST - Urban - Piecewise-Defined Ranking Functions
 *
 * Date: 12.12.2012
 * Author: heizmann@informatik.uni-freiburg.de
 *
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x;
int main() {
   
    while (x <= 10) {
        if (x > 6) {
            x = x + 2;
        }
    }
    return 0;
}
*/

init(X).
while(X):- init(X).
while(X1):- X=<10, X>=7, X1= X+2, while(X).
while(X):- X=<10, X=<6,  while(X).
false:- X>=11, while(X).
