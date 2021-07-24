/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x;
    int y;
    int z;
    int res;
    x = __VERIFIER_nondet_int();
    y = __VERIFIER_nondet_int();
    z = y;
    res = 0;
    
    while (z > 0 && (y == 0 || y > 0 && x > 0))	{
        
        if (y == 0) {
            res = res + 1;
            y = z;
        }
        else {
            x = x + 1;
            y = y - 1;
        }
    }
    
    return 0;
}
 */

init(X,Y,Z).
while(X,Y,Z1, R1):- Z1=Y,R1=0, init(X,Y,Z).
while(X,Y1,Z, R1):- Z>=1, Y=0, R1=R+1,Y1=Z,  while(X,Y,Z,R).
while(X1,Y1,Z, R):- Z>=1, Y>=1, X>=1, X1=X+1, Y1=Y-1, while(X,Y,Z,R).
false:- while(X,Y,Z, R), Z=<0.
false:- while(X,Y,Z, R), Y>=1, X=<0.
false:- while(X,Y,Z, R), Y=< -1, X=<0.
false:- while(X,Y,Z, R), Y=< -1.
