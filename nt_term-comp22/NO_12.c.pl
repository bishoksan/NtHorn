/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    j = 0;
    i = 0;
    
    while (i <= j) {
        if (j-i < 1) { j = j+2; }
        i = i+1;
    }
    
    return 0;
}

*/

init(I,J).
while(I1,J1):- I1=0, J1=0, init(I,J).
while(I1,J1):- I=<J, J-I=<0, J1=J+2, I1=I+1, while(I,J).
while(I1,J):- I=<J, J-I>=1,  I1=I+1, while(I,J).
false:- I-1>=J, while(I,J).
