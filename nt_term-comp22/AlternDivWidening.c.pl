/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int w;
    i = __VERIFIER_nondet_int();
    w = 5;
    
    while (i != 0) {
        if (i < -w) {
            i = i-1;
            i = i*(-1);
        } else {
            if (i > w) {
                i = i+1;
                i = i*(-1);
            } else {
                i = 0;
            }
        }
        w = w+1;
    }

    return 0;
}
 */

init(I).
while(I1, W):- W=5, init(I1).
while(I1, W1):- I>=1, I>= W+1, I1= -I-1, W1= W+1, while(I, W).
while(I1,W1):- I>=1, I=< W-1, I1= 0, W1= W+1, while(I,W).
while(I1, W1):- I=< -1, I =< -W-1, I1= -I+1, W1= W+1, while(I, W).
false:- while(I, W), I=0.
