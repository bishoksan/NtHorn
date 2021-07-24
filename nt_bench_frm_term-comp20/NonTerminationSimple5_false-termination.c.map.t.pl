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
		if (__VERIFIER_nondet_int() != 0) {
			x = x - 1;
		} else {
			x = x + 1;
		}
	}
	return 0;
}
*/

init(X).
while(X, Y):-  init(X).
while(X1, Y1):- X>=0,X1=X-1, Y>=1, while(X, Y).
while(X1, Y1):- X>=0,X1=X-1, Y=< -1, while(X, Y).
while(X1, Y1):- X>=0,X1=X+1,  Y=0, while(X, Y).
false:- X=< -1, while(X,Y).
