/*
 * Date: 2013-12-20
 * Author: leike@informatik.uni-freiburg.de
 *
 * Difficult example for non-termination
 *
 * y = x^log_2(3)
 */
/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

 int x, y;
int main()
{
   
	x = 1;
	y = 1;
	while (x >= 0) {
		x = 2*x;
		y = 3*y;
	}
	return 0;
}
*/

init(X, Y).
while(X, Y):- X=1, Y=1, init(X,Y).
while(X1, Y1):- X>=0, X1=2*X,  Y1=3*Y, while(X, Y).
false:- X=< -1, while(X, Y).
