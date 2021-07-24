/*
 * Date: 2013-12-16
 * Author: leike@informatik.uni-freiburg.de
 *
 * Does not terminate for c >= 0.
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

 int c,x;
int main()
{
	
	while (x >= 0) {
		x = x + c;
	}
    return 0;
}
*/

init(X,Y).
while(X,Y):-  init(X,Y).
while(X1, Y):- X>=0, X1=X+Y, while(X,Y).
false:- X=< -1, while(X,Y).
