/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x;
    int y;
    int tmp;
    int xtmp;
    x = __VERIFIER_nondet_int();
    y = __VERIFIER_nondet_int();
    
    while((y != 0 && x >= 0) && y >= 0) {
        tmp = y;
        xtmp = x;
        
        if (x == y) {
            y = 0;
        }
        else {
            while(xtmp>y) {
                xtmp = xtmp - y;
            }
        }
        
        y = xtmp;
        x = tmp;
    }
    
    return 0;
}

*/

init(X,Y).
while(X,Y, Xt):- init(X,Y).
while(X1,Y1, Xt1):- Y>=1, X>=0, Xt1=X,  X=Y, Y1=X, X1=Y, while(X,Y,Xt).
while_in(X1,Y1, Xt):- Y>=1, X>=0, X-1>=Y, while(X,Y, Xt).
while_in(X1,Y1, Xt):- Y>=1, X>=0, X=<Y-1, while(X,Y, Xt).
while_in(X1,Y1, Xt1):- Xt-1>=Y, Xt1= Xt-Y, while_in(X,Y, Xt).
while(X1,Y1, Xt):- while_in(X,Y, Xt), Xt=< Y, Y1= Xt, X1=Y.

false:- Y=< 0, while(X,Y,Xt).
false:- X=< -1, while(X,Y,Xt).
