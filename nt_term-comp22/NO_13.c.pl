/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    j = 100;
    i = 0;
    
    while (i < j) {
        if (51 < j) { i = i+1; j = j-1; }
        else { i = i-1; j = j+1; }
    }
    
    return 0;
}
*/



init(I,J).
while(I1,J1):- I1=0, J1=100, init(I,J).
while(I1,J1):- I=<J-1, 52=<J, J1=J-1, I1=I+1, while(I,J).
while(I1,J1):- I=<J-1, J=<51,  I1=I-1,J1=J+1, while(I,J).
false:- I>=J, while(I,J).
