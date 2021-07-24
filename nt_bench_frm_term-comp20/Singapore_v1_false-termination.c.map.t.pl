/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
*/

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
int x;
int y;
int main()
{
    
    //x = __VERIFIER_nondet_int();
    //y = __VERIFIER_nondet_int();
    if (x + y > 0) {
        while (x > 0) {
            x = x + x + y;
            y = y - 1;
        }
    }
    return 0;
}
*/

%manual encoding

%our method derives: [[X+Y>=1,-X<0]]

init(X,Y).
while(X,Y):- X+Y>=1,init(X,Y).
while(X1,Y1):- X>=1, X1=2*X+Y, Y1=Y-1 ,while(X,Y).
false:- while(X,Y), X=<0.
false:- init(X,Y), X+Y =< 0.
