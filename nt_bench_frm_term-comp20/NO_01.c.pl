/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int c;
    int i;
    c = (24*6)*6;
    
    if (c <= 10) {
        i = 0;
        while (i < 100) {
            i = i+1;
        }
    }
    else {
        if (c <= 50) {
            i = 0;
            while (i < 101) {
                i = i+1;
            }
        }
        if (c <= 100) {
            i = 0;
            while (i < 102) {
                i = i+1;
            }
        }
        else {
            i = 0;
            while (i < 103) {
                i = i+0;
            }
        }
    }
    
    return 0;
}
 */

init(I).
if1(C1,I):- C1= 24*6*6, init(I).

while1(C,I1):- C=<10,I1=0, if1(C,I).
while1(C,I1):- I=<99, I1=I+1, while1(C,I).
false:- I>=100, while1(C,I).


while2(C,I1):- C>=11,C=<50, I1=0, if1(C,I).
while2(C,I1):- I=<100, I1=I+1, while2(C,I).
if2_exit(C,I):- I>=101, while2(C,I).

if2_exit(C,I):-C>=51, if1(C,I).

while3(C,I1):- C=<100, I1=0, if2_exit(C,I).
while3(C,I1):- I=<101, I1=I+1, while3(C,I).

false:- I>=102, while3(C,I).

while4(C,I1):-C>=101, I1=0, if2_exit(C,I).
while4(C,I1):- I=<102, I1=I+0, while4(C,I).
false:- I>=103, while4(C,I).




