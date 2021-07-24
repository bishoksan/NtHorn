/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 2.4 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: non-terminating (for x=-1, y=0)
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x, y;
int main() {
    while (x < y) {
        x = x + y;
        y = -2*y;
    }
    return 0;
}
*/

init(X,Y).
while(X,Y):- init(X,Y).
while(X1,Y1):- X=< Y-1, X1=X+Y, Y1= -2*Y, while(X,Y).
false:- X>=Y, while(X,Y).
