/*
typedef enum {false, true} bool;

extern int __VERIFIER_nondet_int(void);

int x;
int y;
int main()
{
    //int x;
    //int y;
    //x = __VERIFIER_nondet_int();
    //y = __VERIFIER_nondet_int();
    while (x > 0) {
        x = x + y;
        y = y + 1;
    }
    return 0;
}
*/

/*
our method derives: -1*A<0,-1*A+ -1*B<0,-1*B<2 X>0, A+B>0, B>= -1
*/
init(X,Y).
while(X,Y):- init(X,Y).
while(X1,Y1):- while(X,Y), X>=1, X1=X+Y, Y1=Y+1.
false:- while(X,Y), X=<0. %return stmt
