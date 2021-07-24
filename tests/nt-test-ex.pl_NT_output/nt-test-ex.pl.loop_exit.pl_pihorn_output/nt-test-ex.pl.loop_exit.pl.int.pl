% [8]: while___1(A,B):-[1*A>=1]
% [init___2]: init___2(A,B):-[-1*B>= -99]
% [init___3]: init___3(A,B):-[1*B>=101]
% [5,7,8]: while___4(A,B):-[-1*B>=1,1*A+ -1*B>=3,1*A>=1]
% [6]: if1_exit___5(A,B):-[-1*B>=1]
% [0,2]: if1_exit___6(A,B):-[1*B>=0,1*B>=1]
% [9]: while___7(A,B):-[-1*A>=0]
% []: false:-[]

false :- -1*A>=0,while___7(A,B).
false :- -1*A>=0,while___7(A,B).
false :- -1*A>=0,while___7(A,B).
while___7(A,B) :- 1*B>=1,-1*A>=0,if1_exit___6(A,B).
while___7(A,B) :- -1*B>=1,-1*A>=0,if1_exit___5(A,B).
while___7(A,B) :- 1*A+ -1*B>=3,-1*A>=0,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___4(C,D).
while___7(A,B) :- 1*B>=1,-1*A>=0,if1_exit___6(A,B).
while___7(A,B) :- -1*B>=1,-1*A>=0,if1_exit___5(A,B).
while___7(A,B) :- 1*A+ -1*B>=3,-1*A>=0,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___4(C,D).
while___7(A,B) :- 1*B>=1,-1*A>=0,if1_exit___6(A,B).
while___7(A,B) :- -1*B>=1,-1*A>=0,if1_exit___5(A,B).
while___7(A,B) :- 1*A+ -1*B>=3,-1*A>=0,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___4(C,D).
if1_exit___6(A,B) :- 1*B>=1,1*B+ -1*C= -100,init___3(A,C).
if1_exit___6(A,B) :- 1*B>=1,1*B+1*C=100,init___2(A,C).
if1_exit___6(A,B) :- 1*B>=1,1*B+ -1*C= -100,init___3(A,C).
if1_exit___6(A,B) :- 1*B>=1,1*B+1*C=100,init___2(A,C).
if1_exit___6(A,B) :- 1*B>=1,1*B+ -1*C= -100,init___3(A,C).
if1_exit___6(A,B) :- 1*B>=1,1*B+1*C=100,init___2(A,C).
init___3(A,B) :- 1*B>=101.
init___3(A,B) :- 1*B>=101.
init___3(A,B) :- 1*B>=101.
init___2(A,B) :- -1*B>= -99.
init___2(A,B) :- -1*B>= -99.
init___2(A,B) :- -1*B>= -99.
while___4(A,B) :- 1*A>=1,1*A+ -1*B>=3,-1*B>=1,if1_exit___5(A,B).
while___4(A,B) :- 1*A>=1,1*A+ -1*B>=3,-1*B>=1,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___1(C,D).
while___4(A,B) :- 1*A>=1,1*A+ -1*B>=3,-1*B>=1,if1_exit___5(A,B).
while___4(A,B) :- 1*A>=1,1*A+ -1*B>=3,-1*B>=1,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___1(C,D).
while___4(A,B) :- 1*A>=1,1*A+ -1*B>=3,-1*B>=1,if1_exit___5(A,B).
while___4(A,B) :- 1*A>=1,1*A+ -1*B>=3,-1*B>=1,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___1(C,D).
while___1(A,B) :- 1*A>=1,1*B>=1,if1_exit___6(A,B).
while___1(A,B) :- 1*A>=1,-1*B>=1,if1_exit___5(A,B).
while___1(A,B) :- 1*A>=1,1*A+ -1*B>=3,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___1(C,D).
while___1(A,B) :- 1*A>=1,1*B>=1,if1_exit___6(A,B).
while___1(A,B) :- 1*A>=1,-1*B>=1,if1_exit___5(A,B).
while___1(A,B) :- 1*A>=1,1*A+ -1*B>=3,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___1(C,D).
while___1(A,B) :- 1*A>=1,1*B>=1,if1_exit___6(A,B).
while___1(A,B) :- 1*A>=1,-1*B>=1,if1_exit___5(A,B).
while___1(A,B) :- 1*A>=1,1*A+ -1*B>=3,1*A+ -1*B+ -2*C=1,1*B+ -1*D= -1,while___1(C,D).
