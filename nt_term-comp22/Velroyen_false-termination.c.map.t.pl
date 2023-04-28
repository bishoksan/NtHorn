/*
 * Terminating Program for x < -5 OR 0 <= x <= 30 OR x > 35
 * (from benchmarks of) 2008TAP - Velroyen,Rummer - Non-Termination Checking for Imperative Programs
 *
 * Date: 18.12.2013
 * Author: urban@di.ens.fr
 */
/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x;
int main() {
	//int x;
	while (x!=0) {
	    if (-5 <= x && x <= 35) {
		    if (x < 0) {
			    x = -5;
			} else {
			    if (x > 30) {
				    x = 35;
				} else {
					x = x - 1;
				}
			}
		} else {
		    x = 0;
		}
	}
    return 0;
}
*/

init(X).
while(X):- init(X).
while(X1):-X>=1, X>= -5, X=< 35,X=< -1, X1= -5,  while(X).
while(X1):-X>=1, X>= -5, X=< 35,X>=0, X>=31, X1=35,  while(X).
while(X1):-X>=1, X>= -5, X=< 35,X>=0, X=<30, X1= X-1,  while(X).
while(X1):-X>=1, X=< -6, X=< -1, X1=0,  while(X).
while(X1):-X>=1,  X>= 36,X=< -1, X1=0,  while(X).
while(X1):-X=< -1, X>= -5, X=< 35,X=< -1, X1= -5,  while(X).
while(X1):-X=< -1, X>= -5, X=< 35,X>=0, X>=31, X1=35,  while(X).
while(X1):-X=< -1, X>= -5, X=< 35,X>=0, X=<30, X1= X-1,  while(X).
while(X1):-X=< -1, X=< -6, X=< -1, X1=0,  while(X).
while(X1):-X=< -1,  X>= 36,X=< -1, X1=0,  while(X).
false:- X=0,while(X).
