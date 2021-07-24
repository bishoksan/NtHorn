/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    i = 0;
    
    while (i < 100) {
        j = 0;
        while (j < 1) {
            j = j+0;
        }
        i = i+1;
    }
    
    return 0;
}
*/

init(I,J).
while(I1,J):- I1=0,  init(I,J).
while_in(I,J1):- I=<99, J1=0,  while(I,J).
while_in(I,J1):- J=<0, J1=J+0,  while_in(I,J).
while(I1,J1):- J>=1, I1=I+1,  while_in(I,J).
false:- I>=100, while(I,J).
