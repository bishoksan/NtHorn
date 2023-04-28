/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();

    while (i*i > 9) {
        if (i < 0) {
            i = i-1;
        } else {
            i = i+1;
        }
    }

    return 0;
}
*/

init(I).
wh(I):- init(I).
wh(I1):- wh(I), I*I>=10, I=< -1, I1=I-1.
wh(I1):- wh(I), I*I>=10, I>=0, I1=I+1.
false:- I*I=<9, wh(I).
