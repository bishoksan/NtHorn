/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 2.15 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: non-terminating (for x>0,y>=0)
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x, y;
int main() {
    
    while (x > 0) {
        x = x + y;
    }
    return 0;
}
*/

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y):- X>=1,while(X,Y), X1=X+Y.
false:- while(X,Y), X=<0. %return statements
