/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int a;
    int b;
    int r;
    a = __VERIFIER_nondet_int();
    b = __VERIFIER_nondet_int();
    
    while (b > 0) {
        r =  __VERIFIER_nondet_int();
        b = a - 1 - r;
        a = a - 1 - r;
    }
    
    return 0;
}
*/

init(A,B).
while(A,B,R):-  init(A,B).
while(A1,B1,R1):- B>=1, B1=A-1-R1, A1=A-1-R1, while(A,B,R).
false:- B=< 0, while(A,B,R).
