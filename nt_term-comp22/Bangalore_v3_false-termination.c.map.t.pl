/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
*/

/*
typedef enum {false, true} bool;

//extern int __VERIFIER_nondet_int(void);
int x;
int y;
int main()
{
    
    //  x = __VERIFIER_nondet_int();
    //y = __VERIFIER_nondet_int();
    if (y <= x) {
        while (x >= 0) {
            x = x - y;
        }
    }
    return 0;
}
*/


/*
our method derives: A>=B, A>=0, B=<0. 1*A+ -1*B>=0,-1*A<1,1*B<1
*/
init(A,B).
while(A, B):- init(A,B),A>=B.
while(A1, B):- while(A,B),A>=0, A1=A-B.
if_exit(A,B):- while(A,B),A=< -1.
if_exit(A,B):- init(A,B), A+1=<B.
false:- if_exit(A,B). % return stmt

