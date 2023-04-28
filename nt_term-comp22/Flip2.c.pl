/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    int t;
    i = __VERIFIER_nondet_int();
    j = __VERIFIER_nondet_int();
    t = 0;
    
    while (i > 0 && j > 0) {
        if (i < j) {
            t = i;
            i = j;
            j = t;
        } else {
            if (i > j) {
                j = i;
            } else {
                i = i-1;
            }
        }
    }
    
    return 0;
}
 */

init(I,J).
while(I,J,T):- T=0, init(I,J).
while(I1,J1,T1):- I>=1, J>=1, I=< J-1, T1=I, I1=J, J1=T1, while(I,J,T).
while(I1,J1,T1):- I>=1, J>=1, I-1>= J, J1=I, while(I,J,T).
while(I1,J1,T1):- I>=1, J>=1, I=J,  I1=I-1, while(I,J,T).
false:-I=<0,  while(I,J,T).
false:-J=<0,  while(I,J,T).
