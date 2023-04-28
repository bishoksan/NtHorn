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
			x = x + 1;
		} else {if (__VERIFIER_nondet_int() != 0) {
			x = x + 2;
		} else {if (__VERIFIER_nondet_int() != 0) {
			x = x + 3;
		} else {if (__VERIFIER_nondet_int() != 0) {
			x = x + 4;
		} else {
			x = -1;
		}}}}
	}
	return 0;
}
*/

init(X).
while(X):-   init(X).
while(X1):- X>=0,X1=X+1,  while(X).
while(X1):- X>=0,X1=X+2,  while(X).
while(X1):- X>=0,X1=X+3,  while(X).
while(X1):- X>=0,X1=X+4,  while(X).
while(X1):- X>=0,X1= -1,  while(X).
false:- X=< -1, while(X).

