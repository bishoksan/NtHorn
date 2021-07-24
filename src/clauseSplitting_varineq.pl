:- module(clauseSplitting, [main/1], [dynamic]).

/*

Clause splitting: Given a clause p(X1):- C(X, X1), q(X) and a constraint D over X,X1, clause splitting produces: p(X1):- C(X, X1), D, q(X) and p(X1):- C(X, X1), neg(D), q(X).

The generation of splitting constraints are especially tuned for non-termination.


Use: clauseSplitting -prg <input> -o <output>

*/
% Input:  a program P
% Output: another program equivalent to P

:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(streams)).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(terms_vars)).

%for satisfiability checking and simplifying formulas, interface to yices
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).

:- use_module(chclibs(builtins)).
%:- use_module(chclibs(program_loader)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(common)).
:- use_module(chclibs(canonical)).

:- include(chclibs(get_options)).

%scc and graphs
:- use_module(scc_loops).
:- dynamic(split_constr/3).
:- dynamic(split_cls/2).

recognised_option('-prg',   program(R),[R]).
recognised_option('-o',     outputFile(R),[R]).

:- data flag/1.

main(ArgV) :-
	cleanup,
    yices_init,
	get_options(ArgV,Options,_),
	setOptions(Options,File, OutS),
	load_file(File),
    split_constr_nondet(ConstrsND),
    %write('nd constr '), write(ConstrsND), nl,
    split_constr_scc(SccConstrs),
    split_constrs(ConstrsND,SccConstrs,Constrs),
    save_split_constr(Constrs),
    %write('sp constr '), write(Constrs), nl,
    splitClauses,
    writeClauses(OutS),
    yices_exit,
	close(OutS).

splitClauses:-
    my_clause(H,B,Id),
    (split_constr(H,C,Id)->
    %split clause is present
        append(C,B,Bs),
        assertz(split_cls(H,Bs)),
        conjoin_neg_assert(H,B,C)
    ;
        assertz(split_cls(H,B))
    ),
    fail.
splitClauses.

conjoin_neg_assert(H,B,[X>=0]):-
    !,
    append([X=< -1],B,Bs),
    assertz(split_cls(H,Bs)).
conjoin_neg_assert(H,B,[X>=0|Cs]):-
    append([X=< -1],B,Bs),
    assertz(split_cls(H,Bs)),
    !,
    conjoin_neg_assert(H,B,Cs).



save_split_constr([]).
save_split_constr([(Id,H:-C)|Constrs]):-
    assertz(split_constr(H,C,Id)),
    save_split_constr(Constrs).

split_constrs(ConstrsND,SccConstrs,Constrs):-
    getSccClsIds(SccConstrs,ClsIds),
    collect_split_constrs(ConstrsND,ClsIds, SccConstrs,Constrs).

collect_split_constrs([],_,S,S).
collect_split_constrs([(Id,_ :- _)|NdConstrs],ClsIds, SccConstrs,Constrs):-
    member(Id, ClsIds),
    !,
    collect_split_constrs(NdConstrs,ClsIds, SccConstrs,Constrs).
collect_split_constrs([(Id,H:-C)|NdConstrs],ClsIds, SccConstrs,Constrs):-
    !,
    collect_split_constrs(NdConstrs,ClsIds, [(Id,H:-C)|SccConstrs],Constrs).



getSccClsIds([],[]).
getSccClsIds([(Id,_ :- _)|SccConstrs],[Id|ClsIds]):-
    write('here'), nl,
    getSccClsIds(SccConstrs,ClsIds).

/*
Get split constraints for non-det assigned head variables
non-det var= head vars \ body vars
do not include constraint facts
*/

split_constr_nondet(NdConstrs):-
findall((Id,H:-C),
        (my_clause(H,B,Id), separate_constraints(B,_,Ps), Ps=[_|_],
        varset(H, HVars),
        varset(B, BVars),
        setdiff(HVars,BVars,NDVars),NDVars=[_|_],
        create_constrs(NDVars, C)
        ),
        NdConstrs
        ).

%generates split constraints corresponding for loopheaders
%TODO: remove scc computation by only backedge computation
split_constr_scc(SplitConstr):-
    dependency_graph(Es,Vs),
    scc_graph(Es,Vs,SCCs, BEs), %in topological order
    loopHeaders_frm_bck_edges(BEs, LHeaders),
    rec_head_cls(LHeaders, SCCs, ClsIds),
    gen_split_constrs(ClsIds, SplitConstr).


gen_split_constrs([], []).
gen_split_constrs([(_,ClsId)|ClsIds], Constrs1):-
    gen_split_constr(ClsId, Constr),
    gen_split_constrs(ClsIds, Constrs),
    append(Constr, Constrs, Constrs1).


gen_split_constr([], []).
gen_split_constr([Id|ClsIds], [Constr|Constrs]):-
    gen_split_constr_cls(Id, Constr),
    !,
    gen_split_constr(ClsIds, Constrs).
gen_split_constr([_|ClsIds], Constrs):-
    gen_split_constr(ClsIds, Constrs).


gen_split_constr_cls(Id, (Id, H1:-SplitConstrs1)):-
    my_clause(H,B,Id),
    H=..[_|Xs],
    varset((H,B), ClsVars),
    separate_constraints(B,Cs,_),
    numbervars(ClsVars,0,_),
    bound_constrs_not_entailed_by_body(Xs, ClsVars,  Cs, SplitConstrs),
    %consider only non-empty
    SplitConstrs=[_|_],
    melt((H:-SplitConstrs), (H1:-SplitConstrs1))
    .

%make sure that the added constraints
bound_constrs_not_entailed_by_body([X|Xs], ClsVars, Cs, [X>=0|Scs]):-
    %\+ check_unsat((Cs, X=< -1), ClsVars),
    %\+ check_unsat((Cs, X>=0), ClsVars),
        check_sat((Cs, X=< -1), ClsVars),
        check_sat((Cs, X>=0), ClsVars),
    !,
    bound_constrs_not_entailed_by_body(Xs, ClsVars, [X>=0|Cs], Scs).
bound_constrs_not_entailed_by_body([_|Xs], ClsVars, Cs, Scs):-
    !,
    bound_constrs_not_entailed_by_body(Xs, ClsVars, Cs, Scs).
bound_constrs_not_entailed_by_body([], _, _, []).

check_unsat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_unsat(Formula,VInts).

check_sat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_sat(Formula,VInts).

%checks if some atom in the body of H is in a recursive scc
rec_head_cls([], _, []).
rec_head_cls([P/N|Headers], SCCs, [(P/N,HId)|HIds]):-
    functor(H,P,N),
    findall(Id,
        (my_clause(H,B, Id), separate_constraints(B,_,Ps), member(Pred, Ps), functor(Pred, PA,PN),in_rec_scc(PA/PN, SCCs)),
        HId),
    rec_head_cls(Headers, SCCs, HIds).


in_rec_scc(P/N, [(recursive, SCC)| _]):-
    member(P/N, SCC), !.
in_rec_scc(P/N, [_| SCCs]):-
    !,
    in_rec_scc(P/N, SCCs).
in_rec_scc(_, []):-
    fail.


loopHeaders_frm_bck_edges([], []).
loopHeaders_frm_bck_edges([_-B|R], [B|Rh]):-
    loopHeaders_frm_bck_edges(R, Rh).

%at least one in the loop, assume the first arg is a non-empty list
atleast1inscc([B|_], OrigLoop):-
    functor(B, P, N),
    member(P/N, OrigLoop), !.
atleast1inscc([_|Bs], OrigLoop):-
    !,
    atleast1inscc(Bs, OrigLoop).
atleast1inscc([], _):- fail.


writeSplitConstraints([], _).
writeSplitConstraints([(H:-C, Id)|Cs], OutS):-
    numbervars((H,C), 0, _),
    write(Id), write(': '), write(H), write(':-'), write(C),nl,
    writeSplitConstraints(Cs,OutS).



create_constrs([], []).
create_constrs([V|Vs], [V>=0|Cs]):-
    !,
    create_constrs(Vs,Cs).

	
setOptions(Options,File,  OutS) :-
	( member(program(File),Options) ->
	    true
	; write('No input file given.'), nl,
	  fail
	),
	( member(outputFile(OutFile),Options) ->
	    open(OutFile,write,OutS)
	; OutS=user_output
	).
			
cleanup :-
    retractall(split_constr(_,_,_)),
    retractall(split_cls(_,_)),
	retractall(my_clause(_,_,_)).

writeClauses(S) :-
    split_cls(H,B),
    numbervars((H,B),0,_),
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	writeBodyAtoms(S,B),
	write(S,'.'),
	nl(S),
	fail.
writeClauses(_).
	
writeBodyAtoms(S,[]) :-
	!,
	write(S,'   '),
	write(S,true).
writeBodyAtoms(S,[B]) :-
	!,
	write(S,'   '),
	writeq(S,B).
writeBodyAtoms(S,[B1,B2|Bs]) :-
	write(S,'   '),
	writeq(S,B1),
	write(S,','),
	nl(S),
	writeBodyAtoms(S,[B2|Bs]).
	
			

	

