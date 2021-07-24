/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i != 0) {
        if (i > 5) {
            i = i+1;
        } else {
            if (i < -5) {
                i = i-1;
            } else {
                i = 0;
            }
        }
    }

    return 0;
}
*/

init(I).
while(I1):- init(I1).
while(I1):- I>=1, I >=6, I1= I+1, while(I).
while(I1):- I>=1, I =<5, I1= 0, while(I).
while(I1):- I=< -1, I =< -6, I1= I-1, while(I).
while(I1):- I=< -1, I >= -5, I1= 0, while(I).
false:- while(I), I=0.
