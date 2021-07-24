init(A,B) :- true.
if1_exit(A,B) :- C>=100,B=C-100,init(A,C).
if1_exit(A,B) :- C=<99,B=100-C,init(A,C).
while(A,B) :- B>=1,if1_exit(A,B).
while(A,B) :- B=< -1,if1_exit(A,B).
while(A,B) :- C>=1,A=2*C+D,B=D-1,while(C,D).
init(A,B) :- true.
if1_exit(A,B) :- C>=100,B=C-100,init(A,C).
if1_exit(A,B) :- C=<99,B=100-C,init(A,C).
while(A,B) :- B>=1,if1_exit(A,B).
while(A,B) :- B=< -1,if1_exit(A,B).
while(A,B) :- C>=1,A=2*C+D,B=D-1,while(C,D).
init(A,B) :- true.
if1_exit(A,B) :- C>=100,B=C-100,init(A,C).
if1_exit(A,B) :- C=<99,B=100-C,init(A,C).
while(A,B) :- B>=1,if1_exit(A,B).
while(A,B) :- B=< -1,if1_exit(A,B).
while(A,B) :- C>=1,A=2*C+D,B=D-1,while(C,D).
false :- 1*A>=1,if1_exit(B,A).
false :- -1*A>=1,if1_exit(B,A).
false :- 1*A>=1,if1_exit(B,A).
false :- -1*A>=1,if1_exit(B,A).
false :- 1*A>=1,if1_exit(B,A).
false :- -1*A>=1,if1_exit(B,A).
