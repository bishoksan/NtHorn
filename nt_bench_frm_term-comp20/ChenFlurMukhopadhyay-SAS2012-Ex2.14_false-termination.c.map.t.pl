/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 2.14 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: non-terminating (for x=10k, y=3k, any k>0)
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x, y;
int main() {
    
    while (x > 0 && y > 0) {
        x = 10*y - 2*x;
    }
    return 0;
}
*/

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y):- X>=1, Y>=1, while(X,Y), X1=10*Y - 2*X.
false:- while(X,Y), X=<0. %return statements
false:- while(X,Y), Y=<0. %return statements
