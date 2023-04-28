/*
 * Program from Fig.1 of
 * 2005CAV - Bradley,Manna,Sipma - Linear Ranking with Reachability
 * Modified version that can be nonterminating because we allow that inputs of
 * gcd may be zero.
 *
 * Date: 12.12.2013
 * Author: heizmann@informatik.uni-freiburg.de
 *
 */

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int y1, y2;
int main() {
   
	if (y1 >= 0 && y2 >= 0) {
    	while (y1 != y2) {
	    	if (y1 > y2) {
		    	y1 = y1 - y2;
    		} else {
	    		y2 = y2 - y1;
		    }
	    }
	}
	return 0;
}
*/

init(Y1,Y2).
while(Y1, Y2):-
    Y1>=0, Y2>=0,
    init(Y1,Y2).
if2_entry(Y1,Y1):-
    Y1-1>=Y2,
    while(Y1,Y2).
if2_entry(Y1,Y1):-
    Y1=<Y2-1,
    while(Y1,Y2).
while(Y11,Y2):-
    Y1-1>=Y2,
    Y11=Y1-Y2,
    if2_entry(Y1,Y2).
while(Y1,Y21):-
    Y2>=Y1,
    Y21=Y2-Y1,
    if2_entry(Y1,Y2).
if1_exit(Y1,Y2):- Y1=Y2, while(Y1,Y2).

%terminate when if cond is not satisfied
false:- init(Y1,Y2), Y1=< -1.
false:- init(Y1,Y2), Y2=< -1.
%fall through
false:- if1_exit(Y1,Y2).

