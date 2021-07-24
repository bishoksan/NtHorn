/*typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    int r;
    i = __VERIFIER_nondet_int();
    j = __VERIFIER_nondet_int();
    
    while (i - j >= 1) {
        i = i - __VERIFIER_nondet_int();
        r = __VERIFIER_nondet_int() + 1;
        j = j + r;
    }
    
    return 0;

}
 */

init(I,J,R).
while(I,J,R):-  init(I,J,R).
while(I1,J1,R1):- I-J>=1, I1=I-ND1, R1=ND2+1,J1=J+R1,  while(I,J,R).
false:- I-J=<0, while(I,J,R).
