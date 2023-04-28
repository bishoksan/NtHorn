/*
 * Date: 2013-12-16
 * Author: leike@informatik.uni-freiburg.de
 *
 * Rotates x and y by 90 degrees
 * Does not terminate.
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x;
int y;
int main ()
{
    int oldx;
	
	while (true) {
        oldx = x;
		x = -y;
		y = oldx;
	}
	return 0;
}
*/

init(X,Y).
while(X,Y):- init(X,Y).
while(X1,Y1):- X1= -Y, Y1= X, while(X,Y).
false:- 1=0, while(X,Y).
