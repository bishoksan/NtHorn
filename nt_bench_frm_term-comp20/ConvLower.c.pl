/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i > 5) {
        if (i != 10) {
            i = i-1;
        }
    }
    
    return 0;
}
*/

init(I).
while(I):- init(I).
while(I1):- I>=6, I>=11, I1=I-1, while(I).
while(I):- I>=6, I=<9,  while(I).
false:- while(I), I=<5.
