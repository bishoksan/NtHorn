/*typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i < 0) {
        if (i != -5) {
            i = i+1;
        }
    }
    
    return 0;
}
*/

init(I).
while(I):- init(I).
while(I1):- I=< -1, I>= -4, I1=I+1, while(I).
while(I):- I=< -1, I=< -6, while(I).
false:- I>=0, while(I).
