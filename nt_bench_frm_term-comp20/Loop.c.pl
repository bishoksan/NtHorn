/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int a;
    int b;
    int i;
    a = 5;
    b = 3;
    i = 0;
    
    while (i < 10) { i = i + 0; }
    
    return 0;
}
*/

init(I).
while(I1):- I1=0, init(I).
while(I1):- I=< 9, I1=I+0, while(I).
false:- I>= 10,  while(I).

