% [init___1]: init___1(A,B):-[-1*B>= -99]
% [init___2]: init___2(A,B):-[1*B>=101]
% [6]: if1_exit___3(A,B):-[-1*B>=1]
% [0,2]: if1_exit___4(A,B):-[1*B>=0,1*B>=1]
% []: false:-[]

false :- 1*A>=1,if1_exit___4(B,A).
false :- -1*A>=1,if1_exit___3(B,A).
false :- 1*A>=1,if1_exit___4(B,A).
false :- -1*A>=1,if1_exit___3(B,A).
false :- 1*A>=1,if1_exit___4(B,A).
false :- -1*A>=1,if1_exit___3(B,A).
if1_exit___4(A,B) :- 1*B>=1,1*B+ -1*C= -100,init___2(A,C).
if1_exit___4(A,B) :- 1*B>=1,1*B+1*C=100,init___1(A,C).
if1_exit___4(A,B) :- 1*B>=1,1*B+ -1*C= -100,init___2(A,C).
if1_exit___4(A,B) :- 1*B>=1,1*B+1*C=100,init___1(A,C).
if1_exit___4(A,B) :- 1*B>=1,1*B+ -1*C= -100,init___2(A,C).
if1_exit___4(A,B) :- 1*B>=1,1*B+1*C=100,init___1(A,C).
init___2(A,B) :- 1*B>=101.
init___2(A,B) :- 1*B>=101.
init___2(A,B) :- 1*B>=101.
init___1(A,B) :- -1*B>= -99.
init___1(A,B) :- -1*B>= -99.
init___1(A,B) :- -1*B>= -99.
