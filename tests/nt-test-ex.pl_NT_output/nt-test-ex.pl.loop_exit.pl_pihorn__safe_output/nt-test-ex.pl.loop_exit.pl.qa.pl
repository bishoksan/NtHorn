false :- 
      1*A>=101,
      init___2(B,A).
false :- 
      1*A>=101,
      init___2(B,A).
false :- 
      1*A>=101,
      init___2(B,A).
false :- 
      -1*A>= -99,
      init___1(B,A).
false :- 
      -1*A>= -99,
      init___1(B,A).
false :- 
      -1*A>= -99,
      init___1(B,A).
init___2(A,B) :- 
      1*C>=1,
      1*C+ -1*B= -100,
      if1_exit___4(A,C).
init___1(A,B) :- 
      1*C>=1,
      1*C+1*B=100,
      if1_exit___4(A,C).
init___2(A,B) :- 
      1*C>=1,
      1*C+ -1*B= -100,
      if1_exit___4(A,C).
init___1(A,B) :- 
      1*C>=1,
      1*C+1*B=100,
      if1_exit___4(A,C).
init___2(A,B) :- 
      1*C>=1,
      1*C+ -1*B= -100,
      if1_exit___4(A,C).
init___1(A,B) :- 
      1*C>=1,
      1*C+1*B=100,
      if1_exit___4(A,C).
if1_exit___4(A,B) :- 
      1*B>=1.
if1_exit___3(A,B) :- 
      -1*B>=1.
if1_exit___4(A,B) :- 
      1*B>=1.
if1_exit___3(A,B) :- 
      -1*B>=1.
if1_exit___4(A,B) :- 
      1*B>=1.
if1_exit___3(A,B) :- 
      -1*B>=1.
