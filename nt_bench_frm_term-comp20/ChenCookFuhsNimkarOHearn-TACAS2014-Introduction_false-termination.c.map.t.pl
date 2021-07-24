/*
* Program from the example depicted in the introduction of
* 2014TACAS - Chen,Cook,Fuhs,Nimkar,O’Hearn - Proving Nontermination via Safety
*
* Date: 2014-06-28
* Author: Matthias Heizmann
*
*/

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int k, i;

int main() {
    
    if (k >= 0) {
        // skip
    } else {
        i = -1;
    }
    while (i >= 0) {
        i = __VERIFIER_nondet_int();
    }
    i = 2;
    return 0;
}
*/


init(A, B).
stem(A, B):- A>=0, init(A, B).
stem(A, B1):- init(A, B), A=< -1, B1= -1.

loop(A,B):- stem(A,B).
loop(A,C):- B>=0,  loop(A,B).
false:- B=< -1, loop(A,B).
