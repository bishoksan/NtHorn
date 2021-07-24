/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 2.2 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: nonterminating
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x, y;
int main() {
    while (x < 0) {
        x = x + y;
        y = y - 1;
    }
    return 0;
}
*/


init(X,Y).
while(X,Y):- init(X,Y).
while(X1,Y1):- X=< -1, X1=X+Y, Y1=Y-1, while(X,Y).
false:- X>=0, while(X,Y).

