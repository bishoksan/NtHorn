/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    i = __VERIFIER_nondet_int();
    
    while (i < 10) {
        j = i;
        while (j > 5) {
            j = j+1;
        }
        i = i+1;
    }
    
    return 0;
}
*/


init(I,J).
while(I,J):-  init(I,J).
while_in(I, J1):- while(I,J),I=< 9,J1=I.
while_in(I, J1):- while_in(I,J),J>=6, J1=J+1.
while(I,J):- J=<5, while_in(I,J).

false:- I>=10, while(I,J).
