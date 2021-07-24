/*
 * Date: 2013-12-16
 * Author: leike@informatik.uni-freiburg.de
 *
 * Simple example for non-termination
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

 int x;
int main()
{
	
	while (x >= 0) {
		x = x + 1;
	}
    return 0;
}
*/

init(X).
while(X):-  init(X).
while(X1):- X>=0, X1=X+1, while(X).
false:- X=< -1, while(X).
