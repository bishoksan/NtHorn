/*typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int i;
    int j;
    int k;
    int l;
    int m;
    int a;
    int b;
    i = 0;
    
    while (i < 100) {
        a = i+2;
        j = 0;
        while (j < a) {
            k = i+j+3;
            while (k >= 0) {
                b = i+j+k+4;
                l = 0;
                while (l < b) {
                    m = i+j+k+l+1000;
                    while (m >= 0) {
                        m = m-0;
                    }
                    l = l+1;
                }
                k = k-1;
            }
            j = j+1;
        }
        i = i+1;
    }
    
    return 0;
}
*/

init(A,B,I,J,K,L,M).
while(A,B,I1,J,K,L,M):- I1=0,  init(A,B,I,J,K,L,M).
while2(A1,B,I,J1,K,L,M):-  I=<99, A1=I+2, J1=0, while(A,B,I,J,K,L,M).
while3(A,B,I,J,K1,L,M):-  J=<A-1, K1=I+J+3, while2(A,B,I,J,K,L,M).
while4(A,B1,I,J,K,L1,M):-  K>=0, B1=I+J+K+4,L1=0, while3(A,B,I,J,K,L,M).
while5(A,B,I,J,K,L,M1):-  L=<B-1, M1=I+J+K+L+10000, while4(A,B,I,J,K,L,M).
while5(A,B,I,J,K,L,M):-  M>=0, while5(A,B,I,J,K,L,M).
while4(A,B,I,J,K,L1,M):-  M=< -1, L1=L+1, while5(A,B,I,J,K,L,M).
while3(A,B,I,J,K1,L,M):-  L>=B, K1=K-1, while4(A,B,I,J,K,L,M).
while2(A,B1,I,J1,K,L,M):-  K=< -1, J1=J+1, while3(A,B,I,J,K,L,M).
while(A,B,I,J1,K,L,M):-  J>=A, I1=I+1, while3(A,B,I,J,K,L,M).
false:- while(A,B,I,J,K,L,M),I>=100.





