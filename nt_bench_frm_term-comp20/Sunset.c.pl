/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i > 10) {
        if (i == 25) {
            i = 30;
        }
        if (i <= 30) {
            i = i-1;
        } else {
            i = 20;
        }
    }
    
    return 0;
}
*/

/*
init(I).
while(I):- init(I).
if1(I1):- I>=11, while(I).

if1_exit(I1):- I=25, I1=30, if1(I).
if1_exit(I):- I>=26, if1(I).
if1_exit(I):- I=<24, if1(I).

if2_exit(I1):- I=<30, I1=I-1, if1_exit(I).
if2_exit(I1):- I>=31,I1=20, if1_exit(I).
while(I):- if2_exit(I).

false:- I=<10, while(I).
*/


init(I).
while(I):- init(I).
if1(I):- I>=11, while(I).

if1_exit(I1):- I=25, I1=30, if1(I).
if1_exit(I):- I>=26, if1(I).
if1_exit(I):- I=<24, if1(I).


while(I1):- I=<30, I1=I-1, if1_exit(I).
while(I1):- I>=31,I1=20, if1_exit(I).

false:- I=<10, while(I).
