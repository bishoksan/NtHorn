/*
 * Date: 2014-06-26
 * Author: leike@informatik.uni-freiburg.de
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int c, x;
int main()
{
    
	if (c == 0) {
	    while (x >= 0) {
		    x = x + c;
	    }
    }
	return 0;
}
*/

init(X,C).
while(X, C):- C=0,  init(X,C).
while(X1, C):- X>=0,X1=X+C,  while(X, C).
false:- X=< -1, while(X).

false:- C>=1,  init(X,C).
false:- C=< -1, init(X,C).
