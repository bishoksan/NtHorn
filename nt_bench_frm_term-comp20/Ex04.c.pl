
/*typedef enum { false, true } bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (true) {
        i = i-1;
    }
    
    return 0;
}
*/

init(I).
while(I):- init(I).
while(I1):-  I1=I-1, while(I).
false:- 1=0, while(I).
