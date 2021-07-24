/*
* Date: 06/07/2015
* Created by: Ton Chanh Le (chanhle@comp.nus.edu.sg)
*/

/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);
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
        z = z + x;
    }
    return 0;
}
*/


%manual translation
%our method derives: [[1*A+ -1*B>0],[1*A+ -1*B<0],[1*A+ -1*B>0],[1*A+ -1*B<0]]
init(X,Y,Z).
while(X,Y,Z):- init(X,Y,Z).
while(X,Y,Z):- X>=1, X1= X+Y, Y1= Y+Z, Z1= Z+X1, while(X,Y,Z).
false:- X=<0, while(X,Y,Z). %false if terminates
