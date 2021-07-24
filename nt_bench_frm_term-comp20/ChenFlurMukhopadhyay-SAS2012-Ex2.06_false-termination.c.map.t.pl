/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 2.6 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 * 
 * The authors of the paper claim that this program is terminating, however
 * the program is nonterminating (e.g., initial state x=1 and y=1).
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: terminating, non-linear
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x, y;
int main() {
    int  oldx;
    while (4*x + y > 0) {
        oldx = x;
        x = -2*oldx + 4*y;
        y = 4*oldx;
    }
    return 0;
}
*/

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- 4*X+Y>=1, while(X,Y), X1= -2*X+4*Y, Y1= 4*X.
false:- while(X,Y), 4*X+Y=<0. %return statements
