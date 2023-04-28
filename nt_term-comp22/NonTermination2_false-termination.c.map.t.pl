/*
 * Date: 2014-06-26
 * Author: leike@informatik.uni-freiburg.de
 *
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x;
int main() {
	int  oldx;
	while (x > 1 && x >= 2*oldx) {
		oldx = x;
		x = __VERIFIER_nondet_int();
	}
	return 0;
}
*/

init(X).
while(X, Y):- Y=0, init(X).
while(X1, Y1):- X>=2,X>= 2*Y, Y1=X, while(X, Y).
false:- X=< 1, while(X, Y).
false:- X=< 2*Y -1, while(X, Y).
