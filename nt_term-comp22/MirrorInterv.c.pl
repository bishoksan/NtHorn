/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int range;
    i = __VERIFIER_nondet_int();
    range = 20;
    
    while (-range <= i && i <= range) {
        if (range-i < 5 || range+i < 5) {
            i = i*(-1);
        } else {
            range = range+1;
            i = i-1;
            if (i == 0) {
                range = -1;
            }
        }
    }
    
    return 0;
}
*/



init(I,R).
while(I,R1):- R1=20, init(I,R).
while(I1,R):- -R=<I, I=< R, R-I=<4, I1= -I, while(I,R).
while(I1,R):- -R=<I, I=< R, R+I=<4, I1= -I, while(I,R).
while(I1,R1):- -R=<I, I=< R, R-I>=5,R+I>=5, I1= I-1, I1=0, R1= -1, while(I,R).
while(I1,R1):- -R=<I, I=< R, R-I>=5,R+I>=5, I1= I-1, I-1>=1, R1= R+1, while(I,R).
while(I1,R1):- -R=<I, I=< R, R-I>=5,R+I>=5, I1= I-1, I-1=< -1, R1= R+1, while(I,R).
false:- -R-1>= I, while(I,R).
false:- I-1>= R, while(I,R).
