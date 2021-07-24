/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    i = __VERIFIER_nondet_int();
    
    while (i > 0 && i < 50) {
        if (i < 20) {
            i = i-1;
        }
        if (i > 10) {
            i = i+1;
        }
        if (30 <= i && i <= 40) {
            i = i-1;
        }
        
    }
    
    return 0;
}

*/

init(I).
while(I):- init(I).
if1(I1):- I>=1, I=<49, while(I).
if1_exit(I1):- I=<19, I1=I-1, if1(I).
if1_exit(I):- I>=20,  if1(I).

if2_exit(I1):- I>=11, I1=I+1, if1_exit(I).
if2_exit(I):- I=<10,  if1_exit(I).

if3_exit(I1):- I>=30, I=<40, I1=I-1, if2_exit(I).
if3_exit(I):- I=<29,  if2_exit(I).
if3_exit(I):- I>=41,  if2_exit(I).
while(I):- if3_exit(I).

false:- I=<0, while(I).
false:- I>=50, while(I).
