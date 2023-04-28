/*
 * Program from Example 6 of
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
    
	while (a >= 1 && b >= 1) {
		a = 2*a;
		b = 3*b;
	}
	return 0;
}
*/

init(X,Y).
while(X,Y):- init(X,Y).
while(X1,Y1):- X>=1, Y>=1, X1=2*X, Y1=3*Y, while(X,Y).
false:- X=<0, while(X,Y).
false:- Y=<0, while(X,Y).
