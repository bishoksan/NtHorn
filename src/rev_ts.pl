:- module(rev_ts, _, [assertions,datafacts]).

%! \title Reversal transformation
% based on the paper "Program verification via iterated specialization" 
%by Emanuele De Angelis, Fabio Fioravanti, Alberto Pettorossi, Maurizio Proietti


:- use_module(library(lists), [member/2, append/3, select/3]).
:- use_module(library(aggregates), [setof/3, findall/3, ^ /2]).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(read)).

:- include(cldb).


% ---------------------------------------------------------------------------
% Analysis database

% TODO: Add a package for ':- new_cl(cl)'
% input
:- data cl/3.
cl_reset(cl) :-	retractall_fact(cl(_,_,_)).
cl_add(cl, H, B) :- get_cnum(NewId), assertz_fact(cl(H, B, NewId)).
cl_current(cl, H, B) :- cl(H, B, _).



% (output)
:- data t/3.
cl_reset(t) :- retractall_fact(t(_,_,_)).
cl_add(t, H, B) :- get_cnum(Id), assertz_fact(t(H, B, Id)).
cl_current(t, H, B) :- t(H, B, _).



% ---------------------------------------------------------------------------

% ---------------------------------------------------------------------------
% clause ids

:- data cnum/1. % counter

reset_cnum :-
	retractall_fact(cnum(_)),
	assertz_fact(cnum(0)).

get_cnum(Id) :-
	retract_fact(cnum(I)),
	I1 is I + 1,
	assertz_fact(cnum(I1)),
	%
	number_codes(I, Ic),
	atom_codes(Ia, Ic),
	atom_concat('c', Ia, Id).

% ---------------------------------------------------------------------------

:- data input/2.
cl_reset(input) :- retractall_fact(input(_,_)).
cl_add(input, H, B) :- assertz_fact(input(H, B)).
cl_current(input, H, B) :- input(H, B).

:- data rev/2.
cl_reset(rev) :- retractall_fact(rev(_,_)).
cl_add(rev, H, B) :- assertz_fact(rev(H, B)).
cl_current(rev, H, B) :- rev(H, B).

% ---------------------------------------------------------------------------

% ---------------------------------------------------------------------------

:- export(atom_to_number/2).
atom_to_number(A,B) :-
	atom_codes(A, Cs),
	number_codes(B, Cs).

% ---------------------------------------------------------------------------

:- export([builtin/1, split_body/3, filter_builtin/2, filter_constraints/3]).

builtin(true).
builtin(fail).
%builtin(false).
builtin(_=_).
builtin(_\=_).
builtin(_>=_).
builtin(_=<_).
builtin(_>_).
builtin(_<_).
builtin(read(_,_,_)).
builtin(write(_,_,_,_)).

% Split body X into constaints C and atoms Y
split_body([], [], []).
split_body([X|Xs], [X|Cs], Ys) :-
	builtin(X),
	!,
	split_body(Xs, Cs, Ys).
split_body([X|Xs], Cs, [X|Ys]) :-
	!,
	split_body(Xs, Cs, Ys).

% filter_builtin(G, Theory)
filter_builtin(_=_, integer).
filter_builtin(_\=_, integer). % TODO: not for polyhedra!
filter_builtin(_>=_, integer).
filter_builtin(_=<_, integer).
filter_builtin(_>_, integer).
filter_builtin(_<_, integer).
filter_builtin(read(_,_,_), array).
filter_builtin(write(_,_,_,_), array).

% filter all builtins of a specific theory
filter_constraints([], _Th, []).
filter_constraints([X|Xs], Th, [X|Ys]) :- filter_builtin(X, Th), !,
	filter_constraints(Xs, Th, Ys).
filter_constraints([_|Xs], Th, Ys) :-
	filter_constraints(Xs, Th, Ys).

% ---------------------------------------------------------------------------

% ---------------------------------------------------------------------------
% Messages and output

:- export([dump_cs/1, dump_note/1, dump_term/1, write_cs/2]).

:- use_module(library(write), [numbervars/3]).
:- use_module(library(write), [write/1, writeq/1, write/2, writeq/2]).

dump_cs(Name) :-
	( % (failure-driven loop)
	  cl_current(Name, H, B),
	    display('  ['), display(Name), display('] '),
	    dump_term((H:-B)),
	    fail
	; true
	).

dump_note(C) :-
	display('{'),
	\+ \+ (numbervars(C, 1, _), dump_note0(C)),
	display('}'), nl.

dump_note0(X) :- ( X = [] ; X = [_|_] ), !, dump_note_(X).
dump_note0(X) :- dump_note1(X).

dump_note_([]).
dump_note_([X|Xs]) :-
	dump_note1(X),
	dump_note_(Xs).

dump_note1(X) :- var(X), !, write(X).
dump_note1(''(X)) :- !, writeq(X).
dump_note1(X) :- write(X).

dump_term(C) :- \+ \+ (numbervars(C, 1, _), writeq(C)), nl.

write_cs(Name, OutFile) :-
	open(OutFile,write,OutS),
	writeClauses(Name, OutS),
	close(OutS).

writeClauses(Name, S) :-
	cl_current(Name, A, Body),
	numbervars((A,Body),0,_),
	writeq(S,A),
	write(S,' :- '),
	nl(S),
	writeBodyClauses(Body,S),
	write(S,'.'),
	nl(S),
	fail.
writeClauses(_, _).

writeBodyClauses([],S) :-
	write(S,'      '),
	write(S,true).
writeBodyClauses([A],S) :-
	!,
	write(S,'      '),
	writeq(S,A).
writeBodyClauses([A|As],S) :-
	write(S,'      '),
	writeq(S,A),
	write(S,','),
	nl(S),
	writeBodyClauses(As,S).

% ---------------------------------------------------------------------------

% (for ini-error reversal)
:- data ts/2.
:- data uf/2.
:- data entry/1.

cleanup :-
	retractall_fact(entry(_)),
	retractall_fact(ts(_,_)),
	retractall_fact(uf(_,_)).

% ---------------------------------------------------------------------------
reset_input :-
	cl_reset(input).

load_file(F) :-
	reset_input,
	open(F,read,S),
	remember_all(S),
	close(S).

remember_all(S) :-
	read(S,C),
	( C == end_of_file ->
	    true
	; remember_clause(C),
	  remember_all(S)
	).



remember_clause(A) :- var(A), !. % Drop
remember_clause((:- _)):- !. % Drop all non-execute/specialize clauses
remember_clause((A :- B)) :- !,
	conj2List(B,BL),
	transcl(A,BL,A2,BL2),
	cl_add(input,A2,BL2).
remember_clause(A) :-
	transcl(A,[],A2,BL2),
	cl_add(input,A2,BL2).

conj2List((A, B), [A|R]) :- !,
	conj2List(B,R).
conj2List(A, [A]).
	
	
:- include(cldb).

:- export(check_linear/1).
% Check that the program @var{In} is linear.
% A program is linear if all its clauses has one of the following forms:
%   p(X) :- C1,...,Cn, q(X).
%   p(X) :- C1,...,Cn.
check_linear(In) :-
	( % (failure-driven loop)
	  cl_current(In, _, B),
	    split_body(B, _, Atoms),
	    ( Atoms = [_,_|_] -> % more than 1 atoms
	        throw(clauses_must_be_linear)
	    ; true
	    ),
	    fail
	; true
	).
	

% (translate from other systems)
transcl(H, B, H2, B2) :-
	trans_head(H, H2),
	trans_body(B, B2).

trans_body([],[]).
trans_body([X|Xs],Ys) :-
	conj2List(X,Y2),
	append(Y2,Ys0,Ys),
	trans_body(Xs,Ys0).

trans_head(incorrect, H2) :- !, H2 = false.
trans_head(H, H).



% Revts

%--------------------------------------------------------------------------
:- export(file_revts/3).

main:-
    %ArgV= [In, Out],
    In='revtest.pl',
    Out='revres.pl',
    file_revts(false, In, Out).

file_revts(Entry, InFile, OutFile) :-
     load_file(InFile),
     revts(Entry, input, rev),
     write_cs(rev, OutFile).

revts(Entry, In, Out) :-
    write('Doing reversal transformation'),nl,
	cleanup,
	assertz_fact(entry(Entry)),
	%
	check_linear(In),
    write('linearity checked'), nl,
	cl_reset(Out),
    write('resetting cls'), nl,
	% Create the transition system
	makeTS(In),
    write('making ts'), nl,
	% Unfolding forwards links
	do_unfoldForward,
    write('unfolding'), nl,
        % Undo RW
	undoRW(Out).
    %write('reversing'), nl,
	%verb_cs(Out).

% ---------------------------------------------------------------------------
% MAKETS

makeTS(In) :-
	% (failure-driven loop)
	( cl_current(In, H, B),
	    makeTS_clause(H, B),
	    fail
	; true
	),
	% Template for reversing the transition system
	entry(Entry),
	assertz_fact(ts(Entry, [err(A), reach(A)])),
	assertz_fact(ts(reach(A), [trans(A,B), reach(B)])),
	assertz_fact(ts(reach(A), [ini(A)])).


makeTS_clause(A, B) :- entry(A),
	!,
	split_body(B, Cs, Ds),
	(Ds = [Atom] -> % false:- C,Atom
     Atom =.. List,
     assertz_fact(ts(ini(List), Cs))
    ;%Ds =[]; false:-C
     assertz_fact(ts(err([true]), Cs))
    ).
makeTS_clause(A, B) :-
	split_body(B, Cs, Ds),
	Ds = [], % the rule is a constraint fact
	!,
	A =.. List,
	assertz_fact(ts(err(List), Cs)).
makeTS_clause(A, B) :-
	split_body(B, Cs, Ds),
	Ds = [Atom],
	A =.. List1,
	Atom =.. List2,
	assertz_fact(ts(trans(List2, List1), Cs)).

% ---------------------------------------------------------------------------
% UNFOLD FORWARD

% in: ts/2, out: uf/2
do_unfoldForward :-
	entry(Entry),
	functor(Entry,P,N),
	findBackEdges([P/N],[],_M,[],Bs,[]),
	extractBackPreds(Bs,BPs),
	unfoldForwardEdges(P/N,BPs).

findBackEdges([P|Ps],M0,M3,Anc,Bs0,Bs3) :-
	successors(P,Ss),
	getBackEdges(Ss,P,Anc,Bs0,Bs1),
	marking(Ss,M0,M1,Ss1),
	findBackEdges(Ss1,[P|M1],M2,[P|Anc],Bs1,Bs2),
	findBackEdges(Ps,[P|M2],M3,Anc,Bs2,Bs3).
findBackEdges([],M,M,_,Bs,Bs).

extractBackPreds([(_-P)|Bs],[P|Ps]) :-
	extractBackPreds(Bs,Ps).
extractBackPreds([],[]).

successors(P/N,Ss) :-
	setof(Q/M, [H,B]^(
			functor(H,P,N),
			ts(H,B),
			bodyPred(B,Q/M)),
			Ss),
	!.
successors(_,[]).

bodyPred([B|_],P/N) :-
	\+ builtin(B),
	functor(B,P,N).
bodyPred([_|Bs],Q) :-
	bodyPred(Bs,Q).


getBackEdges([],_,_,Bs,Bs).
getBackEdges([Q|Qs],P,Anc,[P-Q|Bs0],Bs1) :-
	member(Q,[P|Anc]),
	!,
	getBackEdges(Qs,P,Anc,Bs0,Bs1).
getBackEdges([_|Qs],P,Anc,Bs0,Bs1) :-
	getBackEdges(Qs,P,Anc,Bs0,Bs1).

marking([],M,M,[]).
marking([Q|Qs],M0,M1,Ss) :-
	member(Q,M0),
	!,
	marking(Qs,M0,M1,Ss).
marking([Q|Qs],M0,M1,[Q|Ss]) :-
	marking(Qs,[Q|M0],M1,Ss).

unfoldForwardEdges(P/N,BackEs) :-
	ts(H,B),
	functor(H,Q,M),
	member(Q/M,[P/N|BackEs]),
	resultants(H,B,BackEs,Rs),
	add_clauses(Rs),
	fail.
unfoldForwardEdges(_,_).

resultants(H,B,BackEs,Rs) :-
	findall((H:-R), unfoldForward(B,BackEs,R), Rs).

unfoldForward([B|Bs],BackEs,[B|R]) :-
	builtin(B),
	!,
	unfoldForward(Bs,BackEs,R).
unfoldForward([B|Bs],BackEs,[B|R]) :-
	functor(B,P,N),
	member(P/N,BackEs),
	!,
	unfoldForward(Bs,BackEs,R).
unfoldForward([B|Bs],BackEs,R) :-
	ts(B,Body),
	append(Body,Bs,Bs1),
	unfoldForward(Bs1,BackEs,R).
unfoldForward([],_,[]).
	
add_clauses([(H:-B)|Rs]) :-
	assertz_fact(uf(H,B)),
	add_clauses(Rs).
add_clauses([]).
	
% ---------------------------------------------------------------------------
% UNDORW

undoRW(Out) :-
	( % (failure-driven loop) 
	  uf(H, B),
	    undoRW_clause(H, B, Out),
	    fail
	; true
	).

undoRW_clause(A, B, Out) :-
	( undolit(A,Pred) -> 
	    H = Pred
	; H = A
	),
	split_body(B,Cs,Ds),
	( Ds = [Atom], undolit(Atom,Pred1) ->
	    append(Cs, [Pred1], Body)
	; Body = B
	),
	cl_add(Out, H, Body).

undolit(A, Pred) :-
	A=..[_|[M]], Pred=..M, functor(Pred,_,_).

