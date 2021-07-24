/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
*/

/*
typedef enum {false, true} bool;

//extern int __VERIFIER_nondet_int(void);
int x, y, z;
int main()
{
    //int x, y, z;
    //x = __VERIFIER_nondet_int();
    //y = __VERIFIER_nondet_int();
    //z = __VERIFIER_nondet_int();
    while (x > 0) {
        x = x + y;
        y = y + z;
        z = z + 1;
    }
    return 0;
}
*/


/*

our method  derives: init(A,B,C)<- [-1*A<0,-1*B<1,-1*B+ -1*C<1,-1*C<2]
*/

init(X,Y,Z).
while(X,Y, Z):- init(X,Y,Z).
while(X1,Y1, Z1):- while(X,Y, Z), X>=1, X1=X+Y, Y1=Y+Z, Z1=Z+1.
false:- while(X,Y, Z), X=<0. %return stmt

