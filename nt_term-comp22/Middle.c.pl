/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    i = __VERIFIER_nondet_int();
    j = __VERIFIER_nondet_int();
    
    while (i != j) {
        i = i-1;
        j = j+1;
    }
    
    return 0;
}
 */



init(I,J).
while(I,J):- init(I,J).
while(I1, J1):- I-1>=J, I1= I-1, J1= J+1, while(I,J).
while(I1, J1):- I=<J-1, I1= I-1, J1= J+1, while(I,J).
false:- I=J,  while(I,J).
