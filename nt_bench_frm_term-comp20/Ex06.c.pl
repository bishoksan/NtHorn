/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i >= -5 && i <= 5) {
        if (i > 0) {
            i = i-1;
        }
        if (i < 0) {
            i = i+1;
        }
    }
    
    return 0;
}
 */

init(I).
while(I):- init(I).
while(I1):- I>= -5, I=< 5, I>=1, I1=I-1, while(I).
while(I1):- I>= -5, I=< 5, I=< -1, I1=I-1, while(I).
while(I):- I>= -5, I=< 5, I=0,  while(I).
false:- I=< -6, while(I).
false:- I>=6, while(I).
