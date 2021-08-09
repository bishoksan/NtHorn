/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int x;
    int y;
    int res;
    x = __VERIFIER_nondet_int();
    y = 2;
    res = 1;
    
    if (x < 0 || y < 1) { }
    else {
        while (x > y) {
            y = y*y;
            res = 2*res;
        }
    }
    
    return 0;
}
*/

init(X).
if(X,Y,R):- Y=2,R=1, init(X).
while(X,Y,R):- X>=0,Y>=1, if(X,Y,R).
while(X,Y1,R1):- X>Y,Y1=Y*Y,R1=2*R, while(X,Y,R). %non-linear assignment
false:- X < 0, if(X,Y,R).
false:- Y < 1, if(X,Y,R).


