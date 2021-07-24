/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 2.17 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: non-terminating (for x=0, y=11)
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x, y;
int main() {
    
    while (x < 10) {
        x = -y;
        y = y + 1;
    }
    return 0;
}
*/

init(X, Y).
while(X, Y):- init(X, Y).
while(X1, Y1):- X=<9,while(X,Y), X1= -Y, Y1=Y+1.
false:- while(X,Y), X>=10. %return statements
