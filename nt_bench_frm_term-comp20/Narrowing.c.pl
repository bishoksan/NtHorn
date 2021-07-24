/*
typedef enum { false, true } bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int range;
    int up;
    i = __VERIFIER_nondet_int();
    range = 20;
    up = 0;
    
    while (0 <= i && i <= range) {
        if (i == 0) {
            up = 1;
        }
        if (i == range) {
            up = 0;
        }
        if (up == 1) {
            i = i+1;
        }
        if (up == 0) {
            i = i-1;
        }
        if (i == range-2) {
            range = range-1;
        }
    }
    
    return 0;
}
*/

init(I).
while(I,R1,U1):- R1=20, U1=0, init(I).
if1(I,R,U):- I>=0, I=<R,while(I,R,U).
if1_exit(I,R,U1):- if1(I,R,U), I=0, U1=1.
if1_exit(I,R,U):- if1(I,R,U), I>=1.
if1_exit(I,R,U):- if1(I,R,U), I=< -1.
if2_exit(I,R,U1):- if1_exit(I,R,U), I=R, U1=0.
if2_exit(I,R,U):- if1_exit(I,R,U), I-1>=R.
if2_exit(I,R,U):- if1_exit(I,R,U), I=<R-1.

if3_exit(I1,R,U):- if2_exit(I,R,U), U=1, I1=I+1.
if3_exit(I,R,U):- if2_exit(I,R,U), U>=2.
if3_exit(I,R,U):- if2_exit(I,R,U), U=<0.

if4_exit(I1,R,U):- if3_exit(I,R,U), U=0, I1=I-1.
if4_exit(I,R,U):- if3_exit(I,R,U), U>=1.
if4_exit(I,R,U):- if3_exit(I,R,U), U=< -1.

if5_exit(I,R1,U):- if4_exit(I,R,U), I=R-2, R1=R-1.
if5_exit(I,R,U):- if4_exit(I,R,U), I-1>=R-2.
if5_exit(I,R,U):- if4_exit(I,R,U), I=<R-3.
while(I,R,U):- if5_exit(I,R,U).
false:- I=< -1, while(I,R,U).
false:- R=< I-1, while(I,R,U).

