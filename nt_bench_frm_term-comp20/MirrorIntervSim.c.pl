/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i != 0) {
        if (-5 <= i && i <= 35) {
            if (i < 0) {
                i = -5;
            } else {
                if (i > 30) {
                    i = 35;
                } else {
                    i = i-1;
                }	
            }					
        } else {
            i = 0;
        }
    }
    
    return 0;
}
*/

/*
init(I).
while(I):- init(I).
while(I1):- I>=1, -5=<I, I=< 35, I>=31, I1= 35, while(I).
while(I1):- I>=1, -5=<I, I=< 35, I=<30, I1= I-1, while(I).
while(I1):- I>=1,  I>=36, I1= 0, while(I).
while(I1):- I=< -1, -5=<I, I=< 35,  I1= -5, while(I).
while(I1):- I=< -1, -5=<I,   I1= I-1, while(I).
while(I1):- I=< -1,  I=< -6, I1= 0, while(I).

false:- I=0, while(I).
*/

init(I).
while(I):- init(I).
while(I1):- I=< 35, I>=31, I1= 35, while(I).
while(I1):- I>=1, I=<30, I1= I-1, while(I).
while(I1):- I>=36, I1= 0, while(I).
while(I1):- I=< -1, -5=<I,   I1= -5, while(I).
while(I1):- I=< -1, -5=<I,   I1= I-1, while(I).
while(I1):- I=< -1,  I=< -6, I1= 0, while(I).

false:- I=0, while(I).
