:- module(dnf,_).

:- use_module(library(streams)).
:- use_module(library(read)).
:- use_module(library(write)).
:- use_module(library(lists)).
:- use_module(chclibs(ppl_ops)).
:- use_module(library(read_from_string), [read_from_atom/2]).

%interface with yices
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).


% negate(+DNF, -DNF).
% DNF ::= Conj (; Conj)*
% Conj ::= [C] | [C|Conj]
% C ::= X = Y | X >= Y | X =< Y | X > Y | X < Y |

main([C]) :-
	convertString(C,C1),
	numbervars(C1,0,_),
	negate(C1,NC1),
	start_ppl,
	simplify(NC1,_NC),
	end_ppl,
	%write(NC),
	nl.

%given two lists of lists (representing disj) L1 and L2 such that L1 /\ L2 and a set of variables ocurring in them, get a single dnf formula
%equivalent to L1 /\ L2 as a disjunctive sequence F
dnf(L1, L2, _, ([])):-
%empty case
    L1=[[]],
    L2=[[]], !.
dnf(L1, L2, _, ([true])):-
%empty case
    L1=[[true]],
    L2=[[true]], !.
%both having single element case
dnf(L1, L2, _, (C)):-
%single elem case
    %length(L1, 1),
    %length(L2, 1),
    L1 = [A],
    L2 = [B],
    append(A,B,C), !.

dnf(L1, L2, Vs, F):-
    simplify_disj_2_seq(L1, Vs, S1),
    simplify_disj_2_seq(L2, Vs, S2),
    dnf2(S1, S2, F1),
    disj_seq_2_list_list(F1, F2),
    simplify_disj_2_seq(F2, Vs, F).

dnf_list_of_list(L1, L2, Vs, F):-
    simplify_disj_2_seq(L1, Vs, S1),
    simplify_disj_2_seq(L2, Vs, S2),
		write(S1), nl,
		write(S2), nl,
    dnf2(S1, S2, F1),
    disj_seq_2_list_list(F1, F).

%L represents list of lists representing disj

list_of_list_2_seq([A], (A)):-!.
list_of_list_2_seq([A,B], (A;B)):-
    !.
list_of_list_2_seq([A|R], (A;R1)):-
    !,
    list_of_list_2_seq(R, R1).
list_of_list_2_seq([], [false]).


dnf2((C1;C2), (C3;C4), (R1;R2)):-
    !,
    dnf2(C1, (C3;C4), R1),
    dnf2(C2, (C3;C4), R2).
dnf2((C1;C2), C3, (R1;R2)):-
    !,
    dnf2(C1, C3, R1),
    dnf2(C2, C3, R2).
dnf2( C3, (C1;C2),(R1;R2)):-
    !,
    dnf2(C3, C1, R1),
    dnf2(C3, C2, R2).

dnf2(false,_, [false]):-!.
dnf2(_,false, [false]):-!.
dnf2([],_, [false]):-!. %empty list is false
dnf2(_,[], [false]):-!.
dnf2([false],_, [false]):-!.
dnf2(_,[false], [false]):-!.
dnf2(A,B, C):-
    append(A,B,C).


%simplification of disjuncts

%simplifies a dnf formula L to another dnf L1

simplify_dnf_2_dnf([[false]], _, [[false]]):-!.
simplify_dnf_2_dnf([[true]], _, [[true]]):-!.
simplify_dnf_2_dnf(L, Vs, L1):-
   % yices_init,
    %write('dnf entry '), write(L), nl,
    simplify_disj(L, Vs, Seq),
    %write('simplification res '), write(Seq), nl,
    (Seq=false->L1=[[false]]
    ;
    Seq=true->L1=[[true]]
    ;
        disj_seq_2_list_list(Seq, L1)
     %    write('dnf exit '), write(L1), nl
    ).
   %yices_exit.


% simplify_disj(L,_,  L1): L is a list of list representing disjuncts, L1 is
%a seq of disjuncts

simplify_disj([[true]], _, (true)).

simplify_disj(L, Vs, Seq):-
    (L=[]->Seq=(false)
    ;
        filter_trivial_true_false(L, L1),
        simplify_disj_2_seq(L1, Vs, Seq)
    ).


filter_trivial_true_false([X], [X]):-!.
filter_trivial_true_false([C|L], L1):-
    (C=[false]; C=[0=1]; C=[1=0]),
    !,
    filter_trivial_true_false(L, L1).
filter_trivial_true_false([C|_], [[]]):-
    C=[],
    !.
filter_trivial_true_false([C|L], [C|L1]):-
    filter_trivial_true_false(L, L1).

 filter_trivial_true_false([], []).


simplify_disj_2_seq([C],Vs, (false)):-
    unsat(C, Vs), !.
simplify_disj_2_seq([C],_, (C)):-
    !.
simplify_disj_2_seq([], _, (true)).
simplify_disj_2_seq(L,Vs, (false)):-
    listofList2YicesDisj(L, DisjYices),
    unsat(DisjYices, Vs),
    !.
simplify_disj_2_seq([C1, C2|L],Vs, L1):-
    (implies(C1, C2, Vs) ->
        Tmp= C2
    ;
    (implies(C2, C1, Vs) -> Tmp= C1; Tmp=(C1; C2))
    ),
    simplify_disj_2_seq([Tmp|L],Vs, L1).

disj_seq_2_list_list((S1; S2), L):-
    !,
    disj_seq_2_list_list(S1, L1),
    disj_seq_2_list_list(S2, L2),
    append(L1, L2, L).
disj_seq_2_list_list(S1, [S1]):-!.

disj_seq_2_list_list3((S1; S2), L, Vs):-
    !,
    disj_seq_2_list_list3(S1, L1, Vs),
    disj_seq_2_list_list3(S2, L2, Vs),
    append(L1, L2, L).
disj_seq_2_list_list3(S1, [S1], Vs):-
    sat(S1, Vs), !.
disj_seq_2_list_list3(_, [[false]], _):-!.


implies(F1, F2, Vs):-
    unsat((F1, neg(F2)), Vs).

unsat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_unsat(Formula,VInts).

sat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_sat(Formula,VInts).

tautology(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_unsat(neg(Formula),VInts).

listofList2YicesDisj([A], [A]):- !.
listofList2YicesDisj([A|R], (A;R1)):- !,
	listofList2YicesDisj(R, R1).
listofList2YicesDisj([], [false]). %meaning false


is_list(X) :-
        var(X), !,
        fail.
is_list([]).
is_list([_|T]) :-
        is_list(T).

convertString(Q,Q1) :-
	read_from_atom(Q,Q1).


negate([C1], NegC1) :-
	!,
	negateConstraint(C1,NegC1).
negate([C1|C2], NegC1C2) :-
	!,
	negateConstraint(C1,NegC1),
	negate(C2,NegC2),
	disjunct(NegC1,NegC2,NegC1C2).
negate((D1;D2), NegD1D2) :-
	!,
	negate(D1,NegD1),
	negate(D2,NegD2),
	conjunct(NegD1,NegD2,NegD1D2).
negate([], [1=0]) :-
	!.
negate([true], [1=0]) :-
	!.
negate([false], [1=1]) :-
    !.

negateConstraint(X = Y,([X > Y]; [X < Y])).
negateConstraint(X > Y, [X =< Y]).
negateConstraint(X >= Y, [X < Y]).
negateConstraint(X < Y, [X >= Y]).
negateConstraint(X =< Y, [X > Y]).

disjunct((C1;D2),D,(C1;D3)) :-
	!,
	disjunct(D2,D,D3).
disjunct(C1,D1,(C1;D1)).

conjunct((C1;D2),D,D5) :-
	!,
	distribute(D,C1,D3),
	conjunct(D2,D,D4),
	disjunct(D3,D4,D5).
conjunct(C1,D1,D2) :-
	distribute(D1,C1,D2).

distribute((C1;D1),Cs,(C2;D2)) :-
	!,
	append(Cs,C1,C2),
	distribute(D1,Cs,D2).
distribute(C1,Cs,C2) :-
	!,
	append(Cs,C1,C2).

simplify((D1;D2),D5) :-
	makePolyhedron(D1,H),
	getConstraint(H,D3),
	(D3=[0=1] -> D5=D4; D5=(D3;D4)),
	simplify(D2,D4).
simplify(D1,D3) :-
	makePolyhedron(D1,H),
	getConstraint(H,D3),
	!.
simplify(_,false).


%precond pattern (S, not U) where S and U are lists of lists of dnf, DNF is also a list of list of S and not U
%TODO: FIXME, I am very inefficient

precond_patt_2_dnf(([[true]], not(U)), Vs, DNF):-
    !,
    list_of_list_2_seq(U, USeq),
    negate(USeq, UNegated),
    %write('U negated '), write(UNegated), nl,
    disj_seq_2_list_list3(UNegated, DNF1, Vs),
    %write('DNF1 '), write(DNF1), nl,
    simplify_dnf_2_dnf(DNF1, Vs, DNF)
    %write('final dnf true not u '), write(DNF), nl
    .
precond_patt_2_dnf(([true], not(U)), Vs, DNF):-
    !,
    list_of_list_2_seq(U, USeq),
    negate(USeq, UNegated),
    %write('U negated '), write(UNegated), nl,
    disj_seq_2_list_list3(UNegated, DNF1, Vs),
    %write('DNF1 '), write(DNF1), nl,
    simplify_dnf_2_dnf(DNF1, Vs, DNF)
    %write('final dnf true not u '), write(DNF), nl
    .

precond_patt_2_dnf((S, not(U)), Vs, DNF):-
    !,
    %write('ever coming here '), write(S), nl,
    list_of_list_2_seq(U, USeq),
    negate(USeq, UNegated),
    write('U negated '), write(UNegated), nl,
    disj_seq_2_list_list3(UNegated, UNegatedLists, Vs),
    write('U list of list '), write(UNegatedLists), nl,
    dnf_list_of_list(S, UNegatedLists, Vs,  DNF1),
		write('U list of list 2'), write(DNF1), nl,
    simplify_dnf_2_dnf(DNF1, Vs, DNF),
    write('final dnf s not u'), write(DNF), nl
    .
precond_patt_2_dnf(not(U), Vs, DNF):-
    !,
    list_of_list_2_seq(U, USeq),
    negate(USeq, UNegated),
    %write('U negated '), write(UNegated), nl,
    disj_seq_2_list_list3(UNegated, DNF1, Vs),
    simplify_dnf_2_dnf(DNF1, Vs, DNF)
    %write('final dnf not u'), write(DNF), nl
    .
precond_patt_2_dnf(S, Vs, DNF):-
    !,
    simplify_dnf_2_dnf(S, Vs, DNF)
    %write('final dnf only safe'), write(S), nl
    .
