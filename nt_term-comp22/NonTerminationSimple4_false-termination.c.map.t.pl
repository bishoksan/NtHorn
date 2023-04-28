/*
 * Date: 2014-06-26
 * Author: leike@informatik.uni-freiburg.de
 *
 * Does not terminate for y >= 5.
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

 int x, y;
int main()
{
    
	if (y >= 5) {
	    while (x >= 0) {
		    y = y - 1;
    	}
    }
	return 0;
}
*/

init(X,Y).
while(X,Y):- Y>=5, init(X,Y).
while(X,Y1):- X>=0, Y1= Y-1, while(X,Y).
false:- X=< -1, while(X,Y).

false:- Y=<4, init(X,Y).


