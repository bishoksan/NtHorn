/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int a;
    int b;
    a = 1;
    b = 2;
    
    while (a + b < 5) {
        a = a - b;
        b = a + b;
        a = b - a;
    }
    
    return 0;
}

*/

init(A,B).
while(A1,B1):- A1=1,B1=2, init(A,B).
while(A1,B1):- A+B=<4, A11=A-B, B1=A11+B, A1=B1-A11,  while(A,B).
false:- A+B>=5, while(A,B).
