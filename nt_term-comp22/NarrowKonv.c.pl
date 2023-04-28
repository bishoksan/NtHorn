/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int range;
    i = __VERIFIER_nondet_int();
    range = 20;
    
    while (0 <= i && i <= range) {
        if (!(0 == i && i == range)) {
            if (i == range) {
                i = 0;
                range = range-1;
            } else {
                i = i+1;
            }
        }
    }
    
    return 0;
}
*/

init(I).
while(I,R1):- R1=20, init(I).

while(I1,R1):- 0=<I, I=< R, I=< -1,  I=R, I1= 0, R1=R-1, while(I,R).
while(I1,R1):- 0=<I, I=< R, I=< -1,  I-1>=R, I1= I+1, while(I,R).
while(I1,R1):- 0=<I, I=< R, I=< -1,  I=<R-1, I1= I+1, while(I,R).
while(I1,R1):- 0=<I, I=< R, I>=1, I=R, I1= 0, R1=R-1, while(I,R).
while(I1,R):- 0=<I, I=< R, I>=1, I-1>=R, I1= I+1, while(I,R).
while(I1,R):- 0=<I, I=< R, I>=1, I=<R-1, I1= I+1, while(I,R).
while(I1,R):- 0=<I, I=< R, I=<R-1,  I1= I+1, while(I,R).
while(I1,R):- 0=<I, I=< R, I-1>=R,  I1= I+1, while(I,R).
while(I,R):-  I=0,I=R, while(I,R).

false:-  I=< -1, while(I,R).
false:-  I-1>=R, while(I,R).

