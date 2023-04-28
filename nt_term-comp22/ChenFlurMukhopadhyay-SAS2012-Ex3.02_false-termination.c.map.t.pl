/*
 * Date: 2014-06-08
 * Author: leike@informatik.uni-freiburg.de
 *
 *
 * This is Example 3.2 from the test suit used in
 *
 * Termination Proofs for Linear Simple Loops.
 * Hong Yi Chen, Shaked Flur, and Supratik Mukhopadhyay.
 * SAS 2012.
 *
 * The test suite is available at the following URL.
 * https://tigerbytes2.lsu.edu/users/hchen11/lsl/LSL_benchmark.txt
 *
 * Comment: non-terminating (for x>0, y>=0, z>=0)
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x, y,z;
int main() {
    
    while (x > 0) {
        x = x + y;
        y = y + z;
    }
    return 0;
}
*/

init(X, Y,Z).
while(X, Y,Z):- init(X,Y,Z).
while(X1, Y1, Z):- X>=1,while(X,Y,Z), X1= X+Y, Y1=Y+Z.
false:- while(X,Y,Z), X=< -1. %return statements
