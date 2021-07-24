/*
 typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int bob;
    int samantha;
    int temp;
    bob = 13;
    samantha = 17;
    
    while (bob + samantha < 100) {
        temp = bob;
        bob = samantha;
        samantha = temp;
    }
    
    return 0;
}
*/
init(T, B,S).
while(T, B1,S1):- B1=13, S1=17, init(T, B,S).
while(T1, B1,S1):- B+S=<99, T1=B, B1=S, S1=T, while(T, B,S).
false:- B+S>=100, while(T, B,S).
