/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i < 10) {
        if (i != 3) {
            i = i+1;
        }
    }
    
    return 0;
}
 */

init(I).
while(I):- init(I).
while(I1):- I=< 9, I>=4, I1=I+1, while(I).
while(I1):- I=< 9, I=<2, I1=I+1, while(I).
while(I):- I=3, while(I).
false:- I>=10, while(I).

