:- module(clauseSplitting, [main/1], [dynamic]).

/*

Clause splitting: Given a clause p(X1):- C(X, X1), q(X) and a constraint D over X,X1, clause splitting produces: p(X1):- C(X, X1), D, q(X) and p(X1):- C(X, X1), neg(D), q(X).

The generation of splitting constraints are especially tuned for non-termination.


Use: clauseSplitting -prg <input> -o <output>

TODO: assumes that the guards are expressed in terms of prestate vars,
make it general

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
:- include(chclibs(messages)).

%scc and graphs
:- use_module(scc_loops).
:- dynamic(split_constr/4).
:- dynamic(split_cls/2).
:- dynamic(expr/2).

recognised_option('-prg',   program(R),[R]).
recognised_option('-o',     outputFile(R),[R]).
recognised_option('-v', verbose, []).

:- data flag/1.

/*

TODO: 1. nd case

*/


main(ArgV) :-
	cleanup,
    yices_init,
	get_options(ArgV,Options,_),
	setOptions(Options,File, OutS),
    retractall_fact(flag(verbose)),
	( member(verbose, Options) ->
	    assertz_fact(flag(verbose))
	; true
	),
	load_file(File),
    split_constr_scc(SccConstrs),
    verbose_message(['scc split constrs: ']), verbose_message([SccConstrs]),
    getSccClsIds(SccConstrs,ClsIds),
    verbose_message(['Getting scc cls ids ']),
    split_constr_nondet(ClsIds,ConstrsND),
    verbose_message(['Non-det split constr: ']), verbose_message([ConstrsND]),
    append(ConstrsND,SccConstrs,Constrs),
     verbose_message(['Overall split constr: ']), verbose_message([Constrs]),
    save_split_constr(Constrs),
    verbose_message(['Saved split constr in memory ']),
    splitClauses,
    verbose_message(['Cls split done ']),
    writeClauses(OutS),
    verbose_message(['Split clauses written to Out file ']),
    yices_exit,
	close(OutS).

splitClauses:-
    my_clause(H,B,Id),
    separate_constraints(B,_,Ps),
    (split_constr(H,Ps,C,Id)->
    %split clause is present
        append(C,B,Bs),
        assertz(split_cls(H,Bs)),
        conjoin_neg_assert(H,B,C)
    ;
        assertz(split_cls(H,B))
    ),
    fail.
splitClauses.

conjoin_neg_assert(H,B,[E1>=E2]):-
    !,
    append([E1=< E2-1],B,Bs),
    assertz(split_cls(H,Bs)).
conjoin_neg_assert(H,B,[E1=<E2]):-
    !,
    append([E1 -1>= E2],B,Bs),
    assertz(split_cls(H,Bs)).
conjoin_neg_assert(H,B,[C|Cs]):-
    conjoin_neg_assert(H,B,[C]),
    !,
    conjoin_neg_assert(H,B,Cs).


save_split_constr([]).
save_split_constr([(Id,[H|B]:-C)|Constrs]):-
    assertz(split_constr(H,B,C,Id)),
    save_split_constr(Constrs).





getSccClsIds([],[]).
getSccClsIds([(Id,_ :- _)|SccConstrs],[Id|ClsIds]):-
    getSccClsIds(SccConstrs,ClsIds).

/*
Get split constraints for non-det assigned head variables
non-det var= head vars \ body vars
do not include constraint facts
*/

split_constr_nondet_cls(Id,H,Ps,Constr):-
        my_clause(H,B,Id), separate_constraints(B,_,Ps), Ps=[_|_],
        varset(H, HVars),
        varset(B, BVars),
        setdiff(HVars,BVars,NDVars),NDVars=[_|_],
        create_constrs(NDVars, Constr).

split_constr_nondet(ClsIds, NdConstrs):-
    findall((Id,[H|Ps]:-Constr), (my_clause(_,_,Id),\+ member(Id,ClsIds),   split_constr_nondet_cls(Id, H,Ps,Constr)), NdConstrs).

/*
split_constr_nondet(NdConstrs):-
findall((Id,[H|Ps]:-C),
        (my_clause(H,B,Id), separate_constraints(B,_,Ps), Ps=[_|_],
        varset(H, HVars),
        varset(B, BVars),
        setdiff(HVars,BVars,NDVars),NDVars=[_|_],
        create_constrs(NDVars, C)
        ),
        NdConstrs
        ).
*/

%generates split constraints corresponding for loopheaders
%TODO: remove scc computation by only backedge computation
split_constr_scc(SplitConstr):-
    dependency_graph(Es,Vs),
    scc_graph(Es,Vs,SCCs, BEs), %in topological order
    loopHeaders_frm_bck_edges(BEs, LHeaders),
    rec_head_cls(LHeaders, SCCs, ClsIds),
    verbose_message(['loop headers ']), verbose_message([ClsIds]),
    gen_split_constrs(ClsIds, SplitConstr).


gen_split_constrs([], []).
gen_split_constrs([(_,ClsIds)|HeadClsIds], Constrs1):-
    gen_split_constr(ClsIds, Constr),
    gen_split_constrs(HeadClsIds, Constrs),
    append(Constr, Constrs, Constrs1).


gen_split_constr([], []).
gen_split_constr([Id|ClsIds], [Constr|Constrs]):-
        verbose_message(['generating constr for:  ']), verbose_message([Id]),
    gen_split_constr_cls(Id, Constr),
    verbose_message([Constr]),
    !,
    gen_split_constr(ClsIds, Constrs).
gen_split_constr([_|ClsIds], Constrs):-
    gen_split_constr(ClsIds, Constrs).

/*
guards are constraints of the form X>=k and updates are of the form
X1=X+k.
TODO: equality can also be a guard but we have no way of distinguishing in in CHC.
*/
separate_guards_updates([], [], []).
separate_guards_updates([X>=Y|Cs], [X-Y>=0|Guards], Updates):-
    !,
    separate_guards_updates(Cs, Guards, Updates).
separate_guards_updates([X>Y|Cs], [X-Y-1>=0|Guards], Updates):-
    !,
    separate_guards_updates(Cs, Guards, Updates).
separate_guards_updates([X=<Y|Cs], [Y-X>=0|Guards], Updates):-
    !,
    separate_guards_updates(Cs, Guards, Updates).
separate_guards_updates([X<Y|Cs], [Y-X-1>=0|Guards], Updates):-
    !,
    separate_guards_updates(Cs, Guards, Updates).
separate_guards_updates([C|Cs], Guards, [C|Updates]):-
    !,
    separate_guards_updates(Cs, Guards, Updates).

gen_eqn([], [], []).
gen_eqn([H|Hv], [B|Bv], [H=B|R]):-
    gen_eqn(Hv, Bv, R).

%corresponds to a  linear cls of the form p:- c, p
gen_split_constr_cls(Id, RecConstrs):-
    my_clause(H,Body,Id),
    separate_constraints(Body,Cs,[B]), %assume linear clause
    functor(H, P,N),
    functor(B, P,N), %is a recursive clause
    varset((H,B,Cs), ClsVars),
    (var_change_rec_cls(H,B,Cs,Id, ClsVars)->
        gen_split_constr_rec_cls(H,B,Cs,Id, ClsVars, RecConstrs), !
    ;
        !, fail
    ).
gen_split_constr_cls(Id, (Id, [H1|Ps1]:-SplitConstrs1)):-
    my_clause(H,B,Id),
    H=..[_|Xs],
    varset((H,B), ClsVars),
    separate_constraints(B,Cs,Ps),
    numbervars(ClsVars,0,_),
    bound_constrs_not_entailed_by_body(Xs, ClsVars,  Cs, SplitConstrs),
    %consider only non-empty
    SplitConstrs=[_|_],
    melt(([H|Ps]:-SplitConstrs), ([H1|Ps1]:-SplitConstrs1)).


%if both head and body vars are same, then do not generate split constr
var_change_rec_cls(H,B,Cs,_, ClsVars):-
    H=..[_|Hv],
    B=..[_|Bv],
    gen_eqn(Hv, Bv, Eqn),
    numbervars(ClsVars, 0, _),
    %head vars are  same as the body ones
    (check_unsat((Cs, neg(Eqn)), ClsVars)->
        !, fail;
        true
     ).
gen_split_constr_rec_cls(H,B,Cs,Id, ClsVars, (Id, [H1,B1] :- SplitConstrs1)):-
    separate_guards_updates(Cs, Guards, _),
    verbose_message(['guards of ', Id, ' ', Guards]),
    Guards=[_|_], %non-empty guard
    gen_potential_ranking_function(Guards,H,B, Constrs),
    verbose_message(['pot ranking fn ', Constrs]),
    numbervars(ClsVars,0,_),
    potential_ranking_function_not_entailed_by_body(Constrs,ClsVars, Cs, SplitConstrs2),
    verbose_message(['ranking fn not enabled by body ', SplitConstrs2]),
    %add non-det as well
    (split_constr_nondet_cls(Id, H,[B],NDConstr)->
        append(SplitConstrs2, NDConstr, SplitConstrs)
    ;
        SplitConstrs=SplitConstrs2
    ),
    SplitConstrs=[_|_],
    !,
    melt((H,B:-SplitConstrs), (H1,B1:- SplitConstrs1)).

gen_potential_ranking_function([],_,_, []).
gen_potential_ranking_function([Expr_Pre>=_|Guards],H,B, [Expr_Post=< Expr_Pre -1|Constrs]):-
    assertz(expr(B,Expr_Pre)),
    %write('post expr '), write(B), write(':- '),write(Expr_Pre), nl,
    retract(expr(H,Expr_Post)),
    %write('post expr '), write(H), write(':- '),write(Expr_Post), nl,
    gen_potential_ranking_function(Guards,H,B, Constrs).

potential_ranking_function_not_entailed_by_body([E1=< E2|Constrs], ClsVars, Cs, [E1=< E2|Scs]):-
        verbose_message(['current constr ', Cs]),
        check_sat((Cs, E1=< E2), ClsVars),
        check_sat((Cs, E2+1=< E1), ClsVars),
        verbose_message(['current constr ']),
    !,
    potential_ranking_function_not_entailed_by_body(Constrs, ClsVars, [E1=< E2|Cs], Scs).
potential_ranking_function_not_entailed_by_body([_|Constrs], ClsVars, Cs, Scs):-
    !,
    potential_ranking_function_not_entailed_by_body(Constrs, ClsVars, Cs, Scs).
potential_ranking_function_not_entailed_by_body([], _, _, []).



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
    retractall(split_constr(_,_,_,_)),
    retractall(split_cls(_,_)),
	retractall(my_clause(_,_,_)),
    retractall(expr(_,_)).

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
	
			

	

