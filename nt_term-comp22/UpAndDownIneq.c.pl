/*
typedef enum { false, true } bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int up;
    i = __VERIFIER_nondet_int();
    up = 0;
    
    while (0 <= i && i <= 10) {
        if (i >= 10) {
            up = 0;
        }
        if (i <= 0) {
            up = 1;
        }
        if (up >= 1) {
            i = i+1;
        } else {
            i = i-1;
        }
    }
    
    return 0;
}
*/

init(I,U).
while(I,U1):-  U1=0, init(I,U).
if1(I,U):- I>=0, I=<10,while(I,U).
if1_exit(I,U1):- if1(I,U), I>=10, U1=0.
if1_exit(I,U):- if1(I,U), I=< 9.

if2_exit(I,U1):- if1_exit(I,U), I=<0, U1=1.
if2_exit(I,U):- if1_exit(I,U), I>=1.

if3_exit(I1,U):- if2_exit(I,U), U>=1, I1=I+1.
if3_exit(I1,U):- if2_exit(I,U), U=<0, I1=I-1.

while(I,U):- if3_exit(I,U).
false:- I=< -1, while(I,U).
false:- I>=11, while(I,U).
