/*
typedef enum {false,true} bool;

extern int __VERIFIER_nondet_int(void);

int main() {
    int choose;
    int life;
    int death;
    int temp;
    choose = 2;
    life = 13;
    death = 17;

    while (life < death) {
        temp = death;
        death = life + 1;
        life = temp;
        
        if (choose < life || choose < death) {
            life = choose;
        }
    }
    
    return 0;
}
 */

init(C,L,D).
while(C,L,D):- C=2, L=13, D=17, init(C,L,D).
while(C,L1,D1):- L=< D -1, D1=L+1, L1= C, C=< L-1, while(C,L,D).
while(C,L1,D1):- L=< D -1, D1=L+1, L1= C, C=< D-1, while(C,L,D).
while(C,L1,D1):- L=< D -1, D1=L+1,  C>= L, C>=D, L1=D, while(C,L,D).
false:- L>=D, while(C,L,D).

