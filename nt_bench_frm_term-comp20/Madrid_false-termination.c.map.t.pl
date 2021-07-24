/*
 * Date: 2013-05-02
 * Author: heizmann@informatik.uni-freiburg.de
 *
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x;
int main()
{
    
	x = 7;
	while (true) {
		x = 2;
	}
	return 0;
}
*/


init(X):-X=7.
while(X):- init(X).
while(X1):- X1=2, while(X).
false:- 0=1, while(X,Y).
