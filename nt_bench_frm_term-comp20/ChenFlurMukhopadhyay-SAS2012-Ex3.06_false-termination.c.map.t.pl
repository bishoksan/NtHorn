/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 3.6 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: non-terminating (for x=-1, y=1, z=-1)
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x, y, z;
int main() {
    
    while (x < 0) {
        x = x + z;
        z = -2*y;
        y = y + 1;
    }
    return 0;
}
*/

init(X, Y,Z).
while(X, Y,Z):- init(X,Y,Z).
while(X1, Y1, Z1):- X=< -1,while(X,Y,Z), X1= X+Z, Z1= -2*Y, Y1=Y+1.
false:- while(X,Y,Z), X>= 0. %return statements
