
/*typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int n;
    int i;
    int j;
    int t;
    n = __VERIFIER_nondet_int();
    i = 0;
    j = 1;
    t = 0;
    
    while (j != n) {
        t = j+i;
        i = j;
        j = t;
    }
	
    return 0;
}
 */

init(I,J,N,T).
while(I1,J1,N,T1):- I1=0, J1=1, T1=0, init(I,J,N,T).
while(I1,J1,N,T1):- J-1>=N,T1=J+1, I1=T, J1=T, while(I,J,N,T).
while(I1,J1,N,T1):- J=<N-1,T1=J+1, I1=T, J1=T, while(I,J,N,T).
false:-J=N,  while(I,J,N,T).

