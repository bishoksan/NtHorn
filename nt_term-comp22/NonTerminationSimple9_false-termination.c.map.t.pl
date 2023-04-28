/*
 * Date: 2014-06-26
 * Author: leike@informatik.uni-freiburg.de
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x;
int main()
{
	
	while (x >= 0) {
		x = x + __VERIFIER_nondet_int();
	}
	return 0;
}
*/

init(X).
while(X, Y):- init(X).
while(X, Y1):- X>=0, X1= X+Y, while(X, Y).
false:- X=< -1, while(X,Y).
