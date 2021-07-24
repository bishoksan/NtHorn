/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int n;
    int sum;
    n = __VERIFIER_nondet_int();
    sum = 0;
    
    while (n != 0) {
        sum = sum + n;
        n = n - 1;
    }

    return 0;
}
*/

init(N,S).
while(N,S1):- S1=0, init(N,S).
while(N1,S1):- N>=1,S1= S+N, N1= N-1, while(N,S).
while(N1,S1):- N=< -1,S1= S+N, N1= N-1, while(N,S).
false:- N=0, while(N,S).
