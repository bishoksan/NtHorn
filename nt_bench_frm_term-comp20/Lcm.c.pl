
/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int a;
    int b;
    int am;
    int bm;
    am = a;
    bm = b;
    
    while (am != bm) {
        if (am > bm) {
            bm = bm+b;
        } else {
            am = am+a;
        }
    }

    return 0;
}
*/

%local vars are not initialised
init(A,B).
while(A,B, Am, Bm):-Am=A, Bm=B, init(A,B).
while(A,B, Am, Bm1):- Am-1>=Bm, Bm1=Bm+B, while(A,B,Am, Bm).
while(A,B, Am1, Bm):- Am=<Bm-1, Am1=Am+A, while(A,B,Am, Bm).
false:- Am=Bm, while(A,B, Am, Bm).
