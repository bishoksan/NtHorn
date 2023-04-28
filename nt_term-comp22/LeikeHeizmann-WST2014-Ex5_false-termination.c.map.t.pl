/*
 * Program from Example 5 of
 * 2014WST - Leike, Heizmann - Geometric Series as Nontermination Arguments for Linear Lasso Programs
 *
 * Date: 2014-06-29
 * Author: Jan Leike
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int a, b;
int main() {
    int  olda;
	
	while (a >= 7) {
		olda = a;
		a = b;
		b = olda + 1;
		// b = a + 1; is a typo in the paper
	}
	return 0;
}
*/

init(X,Y).
while(X,Y):- init(X,Y).
while(X1,Y1):- X>=7, X1=Y, Y1=X+1, while(X,Y).
false:- X=<6, while(X,Y).
