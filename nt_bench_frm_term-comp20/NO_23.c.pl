/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = 0;
    
    while (i < 100) {
        if (i < 50) { i = 51; }
        else { i = 49; }
    }
    
    return 0;
}
 */

init(I).
while(I1):- I1=0, init(I).
while(I1):- I=< 99,I=<49, I1=51,  while(I).
while(I1):- I=< 99,I>=50, I1=49,  while(I).
false:- I>=100, while(I).
