/*
 * Date: 2013-12-16
 * Author: leike@informatik.uni-freiburg.de
 *
 * Very simple example for non-termination
 */
/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int main()
{
	while (true) {
		// do nothing
	}
	return 0;
}
*/

init.
while:- init.
while:- while.
false:- 1=0, while.
