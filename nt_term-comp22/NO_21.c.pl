/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = 0;
    
    while (i < 100) {
        i = i+1;
        i = i-1;
    }
    
    return 0;
}
*/

init(I).
while(I1):- I1=0, init(I).
while(I):- I=< 99,  while(I).
false:- I>=100, while(I).
