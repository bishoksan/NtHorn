/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int a;
    int b;
    int c;
    int r;
    a = __VERIFIER_nondet_int();
    b = __VERIFIER_nondet_int();
    c = __VERIFIER_nondet_int();
    
    while ( (b - c >= 1) && (a == c)) {
        r = __VERIFIER_nondet_int();
        b = 10;
        c = c + 1 + r;
        a = c;
    }
    
    return 0;
}
*/

init(A,B,C).
while(A,B,C, R):- init(A,B,C).
while(A1,B1,C1, R1):- B-C>=1, A=C,  B1=10, C1= C+1+R1, A1=C1, while(A,B,C,R).
false:- B-C=< 0, while(A,B,C,R).
false:- A>=C+1, while(A,B,C,R).
false:- A=<C-1, while(A,B,C,R).
