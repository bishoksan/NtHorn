/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    i = __VERIFIER_nondet_int();
    j = __VERIFIER_nondet_int();
    
    while (true) {
        if (i < j) {
            i = i+4;
        } else {
            j = j+1;
            i = i+2;
        }
    }
	
    return 0;
}
 */

init(I,J).
while(I,J):- init(I,J).
while(I1,J):- I=< J-1, I1=I+4, while(I,J).
while(I1,J):- I>=J, I1=I+2, J=J+1, while(I,J).
false:- 1=0, while(_,_).
