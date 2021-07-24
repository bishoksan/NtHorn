/*
NonTerm-Horn: It is a tool for inferring (sufficient) preconditions for non-termiantion of a program. The tool converts non-termination analysis problem into verification problems of Horn clauses. The overall procedure is summaried as follows.

It takes each loop individually: (i) generates sufficient preconditions on the initial states of the program that does not exit the loop; (ii) checks from the generated preconditions if the loop-entry is reachable. If this is the case, then the program is non-terminating. The sufficient precondition that is reachable at the entry of the loop is precondition for non-termiantion.

*/

:- module(precond_non_termination, _, [datafacts,dynamic]).

% Input: a set of Horn clauses P with distinguished init, false and safe predicates
%plus two clauses (spec:- false. and spec:- safe.) specifying specs.
% Output: safety/unsafety suff preconditions for P wrt safe and false predicates

:- use_module(library(streams)).
:- use_module(library(format), [format/2, format/3]).
:- use_module(library(system_extra), [mkpath/1,mktempdir_in_tmp/2, rmtempdir/1]).
:- use_module(library(system)). % mktemp_in_tmp is available here
:- use_module(library(pathnames), [path_basename/2, path_concat/3, path_split/3]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(engine(runtime_control), [statistics/2]).
:- use_module(library(lists)).
:- use_module(library(terms_vars)).
:- use_module(library(aggregates)).
:- use_module(library(strings)).

%scc and graphs
:- use_module(scc_loops).


:- use_module(chclibs(thresholds1), [main/1]).
:- use_module(chclibs(load_simple)).
%:- use_module(chclibs(program_loader)).
:- use_module(chclibs(cpascc), [main/1]).
:- use_module(chclibs(qa), [main/1]).
:- use_module(chclibs(common)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(linearize)).

%property based abstraction using PE
:- use_module(props).
:- use_module(peunf).
:- use_module(precond).

%clause splitting

:-use_module(clauseSplitting).

%insert preconditions
:- use_module(insertPrecond).
:- use_module(constrainInitStates).
:- use_module(dnf).
:- use_module(insertNegProps,[main/1]).

%for satisfiability checking and simplifying formulas, interface to yices
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).

:- use_module(raf, [main/1]).
:- use_module(counterExampleYices, [counterExample/2, readCex/2, separateLinearConstraints/3]).
:- use_module(insertProps, [main/1]).
:- use_module(genfta, [main/1]).
:- use_module(splitClauseIds, [main/1]).
:- use_module(ftaRefine, [main/1]).
:- use_module(interpolantAutomaton_bck, [main/1]).
:- use_module(checkFalseInFile, [checkForFalse/2]).
:- use_module(checkAnnotationsInFile, [checkForAnnotations/2]).
%:- use_module(rev_ts, [file_revts/3]).

:- use_module(library(write)).
:- use_module(library(read)).

:- use_module(library(source_tree), [remove_dir/1]).

:- use_module(library(process)). % invoking external processes

:- include(chclibs(get_options)).
:- include(chclibs(messages)).

% stores output of the tool
logfile_pi_horn('result_pi_horn.txt').
logfile_non_term('result_non-termination.txt').


% ---------------------------------------------------------------------------
% Main
% ---------------------------------------------------------------------------

:- data flag/1.
:- data opt_debug_temps/0.


% initial constraints on feasible traces rooted at predicate false
:- dynamic trConstr/1.
:- dynamic trConstrAcc/1. %disj of trconstr, for checking sat

% initial constraints on feasible traces rooted at predicate safe
:- dynamic trConstrSafe/1.
:- dynamic trConstrSafeAcc/1.

%records intersection of safe and unsafe over-approx, initiallly the initial states
:- dynamic mixState/1. %set of vars will be init state vars

%precond, is a pair of safe and unsafe over-approx [(S, U), ..., (Sn, Un)]
%suff safe precond is S\U \/ ...\/ Sn\Un
%suff unsafe precond is U\s \/ ...\/ Un\Sn
:- dynamic precond/1.

:- dynamic(preserveInitPred/0). %var that controls unfolding of init predicate




% status of the precond
:- dynamic optimal/0.

% printing output of non-termination-horn
displayHelpMenu:-
	help_msg(Str),
	format(user_error, "~s~n", [Str]).

help_msg(
"Usage: non-termination-horn <prog> [<Options>]

Options:
 -help        display this help menu
 -v           verbose
 -raf         enable redundant argument filtering
 -int         uses interpolant automaton for trace generalisation during refinement
 -model       show model
 -array       enable array constraints
 -sp          only horn specialization
 -itr N       limit abstract refine iterations 

 -debug-temps    keep files for intermediate passes (debug)
 -init           preserves init predicates during transformations
 -pe           apply partial evaluation as a preprocessing
 -clssplit     apply clause splitting
").

recognised_option('-help',  help, []).
recognised_option('-model', model, []).
recognised_option('-v', verbose, []).
recognised_option('-raf', raf, []).
recognised_option('-pe', pe, []).
recognised_option('-clssplit', clssplit, []).
recognised_option('-unfoldF', uf, []). %unfold forward
recognised_option('-int',   int, []).
recognised_option('-array', array, []).
recognised_option('-sp',    horn_specialise(F), [F]).
recognised_option('-itr',   bounded(N), [N]).
recognised_option('-debug-temps', debug_temps, []).
recognised_option('-init', preserveInitPred,[]).

main(ArgV) :-
	precond_non_termination:get_options(ArgV,Options,Args0),
	( member(help, Options) ->
	    displayHelpMenu
	; \+ Args0 = [_] -> % wrong args
	    displayHelpMenu
	; Args0 = [F],
	  precond_non_termination:cleanup,
	  main_(Options, F)
	).

cleanup :-
	retractall_fact(precond_non_termination:flag(_)),
    retractall_fact(trConstrSafe(_)),
    retractall_fact(trConstrSafeAcc(_)),
    retractall_fact(trConstr(_)),
    retractall_fact(trConstrAcc(_)),
    retractall_fact(optimal),
    retractall_fact(precond(_)),
    retractall_fact(preserveInitPred),
    retractall_fact(mixState(_)).

cleanup_trace_constr:-
	retractall_fact(trConstrSafe(_)),
    retractall_fact(trConstrSafeAcc(_)),
    retractall_fact(trConstr(_)),
    retractall_fact(trConstrAcc(_)).

initialise_tr_constrs:-
    assertz_fact(trConstr([false])),
    assertz_fact(trConstrAcc([false])),
    assertz_fact(trConstrSafe([false])),
    assertz_fact(trConstrSafeAcc([false])).

main_(Options, Prog) :-
	member(horn_specialise(OFile), Options),
	!,
	hornSpecialise(Prog, OFile).
main_(Options, Prog) :-
	( member(int, Options) ->
	    WithInterpolant = yes
	; WithInterpolant = no
	),
	( member(array, Options) ->
	    assertz_fact(precond_non_termination:flag(array))
	; true
	),
	( member(raf, Options) ->
	    assertz_fact(precond_non_termination:flag(raf))
	; true
	),
	( member(pe, Options) ->
	    assertz_fact(precond_non_termination:flag(pe))
	; true
	),
	( member(clssplit, Options) ->
	    assertz_fact(precond_non_termination:flag(clssplit))
	; true
	),
	( member(uf, Options) ->
	    assertz_fact(precond_non_termination:flag(uf))
	; true
	),
	( member(bounded(N), Options) ->
	    convert2num(N,N1),
	    Bounded = bounded(N1)
	; Bounded = unbounded
	),
	( member(debug_temps, Options) ->
	    assertz_fact(opt_debug_temps)
	; true
	),
	retractall_fact(precond_non_termination:flag(verbose)),
	( member(verbose, Options) ->
	    assertz_fact(precond_non_termination:flag(verbose))
	; true
	),
    (member(preserveInitPred,Options), assert(preserveInitPred)
        ; true),
	analyse_non_term(Prog, WithInterpolant, Bounded).

% ---------------------------------------------------------------------------
% property based abstraction using PE
% ---------------------------------------------------------------------------
pe(Prog,  F_Threshold, OutputFile):-
    props:main(['-prg', Prog, '-o', F_Threshold, '-l','3']), % -l 3 means head and call constraints
    (preserveInitPred ->
        % init flag preserves init clauses from getting unfolded
        peunf:main(['-prg', Prog,'-init' , '-entry',false, '-props', F_Threshold, '-o', OutputFile])
    ;
        peunf:main(['-prg', Prog, '-entry',false, '-props', F_Threshold, '-o', OutputFile])
    ).

% ---------------------------------------------------------------------------
% Horn clause pre-processing
% ---------------------------------------------------------------------------

preProcessHorn(Prog, F_Int, F_QA, QACPA, F_WidenPoints, F_Threshold, OutputFile):-
	precond_non_termination:verbose_opts(VerbOpts),
    ( precond_non_termination:flag(pe) ->
        verbose_message(['property based abstraction using PE']),
    pe(Prog,  F_Threshold,F_Int),
    Prog2Int = F_Int
    ;
        Prog2Int =Prog
    ),
	verbose_message(['Computing query-answer transformation']),
	qa:main([Prog2Int, '-query', 'false', '-ans',  '-o', F_QA]),
    verbose_message(['Reversing transition system']),
	%rev_ts:file_revts(false, Prog2Int,F_QA),
	verbose_message(['Computing widening thresholds for QA program']),
    write('threshold dumpted at '), write(F_Threshold), nl,
    %props:main(['-prg', Prog, '-o', F_Threshold]),
	%thresholds1:main(['-prg', F_QA, '-a','-o', F_Threshold]),
    thresholds1:main(['-prg', F_QA,'-o', F_Threshold]),
	verbose_message(['Analyse QA program']),
    %write('threshold dumped here but why '), write(F_QA), nl,
	cpascc:main(['-prg', F_QA, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints,'-threshold', F_Threshold, '-o', QACPA|VerbOpts]),
    %write('I passed this point'), nl,
    %use the following line if revts is used, that is, inv for the orig is neg of the inv for revts
    insertNegProps:main(['-prg', Prog2Int, '-props', QACPA, '-o', OutputFile|[]]).
	%insertProps:main(['-prg', Prog2Int, '-props', QACPA, '-o', OutputFile|[]]).

% ---------------------------------------------------------------------------
% Analysis using CPA
% ---------------------------------------------------------------------------

verifyCPA(Prog, F_Int, F_QA, QACPA, F_CPA, OutputFile, F_WidenPoints, F_Traceterm, F_Threshold,Result) :-
	precond_non_termination:verbose_opts(VerbOpts),
    verbose_message(['preprocessing clauses']),
	preProcessHorn(Prog, F_Int, F_QA, QACPA, F_WidenPoints, F_Threshold, OutputFile),
	verbose_message(['Checking for the presence of false clauses']),
	checkFalseInFile:checkForFalse(OutputFile, Result1),
	( Result1=safe -> % no (false :- ...)
	    % TODO:{arrays} "if there is no trace for false in A_P' then return safe" (is it equivalent?)
	    Result=safe
	;
	    verbose_message(['Computing widening thresholds for PE program']),
	    thresholds1:main(['-prg', OutputFile,'-a', '-o', F_Threshold]),
	    verbose_message(['Analyse PE program']),
	    cpascc:main(['-prg', OutputFile, '-withwut', 'bounded', '-wfunc', 'h79', '-widenpoints',F_WidenPoints, '-threshold', F_Threshold, '-cex', F_Traceterm, '-o', F_CPA]),
	    verbose_message(['Analysing counterexample']),
        counterExampleYices:counterExample([OutputFile, F_Traceterm|VerbOpts], Result)
	).

% ---------------------------------------------------------------------------

:- use_module(library(system), [file_exists/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

% Find determinise.jar in the same directory as the executable
% or in the sources.
determinise_jar(Path) :-
	( current_executable(ExecPath),
	  path_split(ExecPath, Dir, _)
	; bundle_path('RAHFT', 'src', Dir)
	),
	path_concat(Dir, 'determinise.jar', Jar),
	file_exists(Jar),
	!,
	Path = Jar.

% ---------------------------------------------------------------------------
% Refinement
% ---------------------------------------------------------------------------

refineHorn(F_SP, F_FTA, F_DFTA, F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant):-
	precond_non_termination:verbose_opts(VerbOpts),
        verbose_message(['Generate FTA from program and error trace']),
        genfta:main(['-prg', F_SP, '-trace', F_TRACETERM, '-o', F_FTA]),
        ( WithInterpolant=yes ->
            verbose_message(['Computing interpolant automaton from an error trace']),
            interpolantAutomaton_bck:main(['-prg', F_SP,  '-trace',  F_TRACETERM, '-o',  F_FTA|VerbOpts])
	; true
        ),
	%
	determinise_jar(DeterminiseJar),
	( precond_non_termination:flag(verbose) ->
	    DeterminiseOpts = []
	; DeterminiseOpts = [stdout(null)]
	),
	process_call(path(java), ['-jar', DeterminiseJar, F_FTA, '-nodc', '-show', '-o', F_DFTA], DeterminiseOpts),
	%
        verbose_message(['Find disjoint clauses']),
        splitClauseIds:main(['-prg', F_SP, '-o', F_SPLIT]),
        verbose_message(['Refining using DFTA']),
        ftaRefine:main(['-prg', F_SP, '-fta', F_DFTA, '-split', F_SPLIT, F_SPLIT, '-o', F_REFINE]).

% ---------------------------------------------------------------------------
% printing output of RAHFT
% ---------------------------------------------------------------------------

printRahftOutput(_, Prog, SafeStatus, UnSafeStatus, Iteration, InterpolantOption, Time):-
	%printRahftOutput_(LogS, Prog, SafeStatus, UnSafeStatus, Iteration, InterpolantOption, Time),
	printRahftOutput_(user_output, Prog, SafeStatus, UnSafeStatus, Iteration, InterpolantOption, Time).


%[solver(rahft), program('addition.nts.pl'), safety(safe), iteration(0), time(129.487,ms), opt('-int')].
printRahftOutput_(LogS, Prog, SafeStatus, UnSafeStatus, Iteration, InterpolantOption, Time):-
	format(LogS, '[solver(Pi-Horn), ', []),
	format(LogS, 'program(~q), ', [Prog]),
	format(LogS, 'precond-safe(~w), ', [SafeStatus]),
    format(LogS, 'precond-unsafe(~w), ', [UnSafeStatus]),
	format(LogS, 'iteration(~w), ', [Iteration]),

    (InterpolantOption=yes ->
        I = '-int'
    ; I = '-noint'),

    format(LogS, 'option(~q), ', [I]),
	format(LogS, 'time(~w, ms)]. ~n', [Time]).

printNonTermOutput(LogS, Prog, PrecondNT,  InterpolantOption, Time):-
    printNonTermOutput_(LogS, Prog, PrecondNT,  InterpolantOption, Time),
    printNonTermOutput_(user_output, Prog, PrecondNT,  InterpolantOption, Time).

printNonTermOutput_(LogS, Prog, PrecondNT,  InterpolantOption, Time):-
	format(LogS, '[solver(Non-Termination-Horn), ', []),
	format(LogS, 'program(~q), ', [Prog]),
	format(LogS, 'status(~w), ', [PrecondNT]),

    (InterpolantOption=yes ->
        I = '-int'
    ; I = '-noint'),

    format(LogS, 'option(~q), ', [I]),
	format(LogS, 'time(~w, ms)]. ~n', [Time]).




/*
only those with init/ initXXX predicates are initial states
*/
record_initial_states(Prog, InitStates, InitVars):-
    precond(Prog,Ls),
    getInitStateAndVars(Ls, InitStates, [], InitVars).


getInitStateAndVars([(P,C)], [C], Vars, AllVars):-
    P=..[_|Xs],
    setunion(Xs, Vars, AllVars).
getInitStateAndVars([], [[false]], Vars, Vars).
getInitStateAndVars([(P,C)|Ls], [C|Cs], Vars, AllVars):-
    P=..[_|Xs],
    setunion(Xs, Vars, Vars1),
    getInitStateAndVars(Ls, Cs, Vars1, AllVars).


%the original pred is "orig_init"
record_original_init_state(P, Formula):-
    load_file(P),
    get_orig_init_list(Formula).

get_orig_init_list(Formula):-
    findall(C,
        (my_clause(H, B, _), functor(H, 'orig_init', _),  separate_constraints(B, C, []), numbervars((H,B), 0, _)),
        Formula).


getPrecond(Precond):-
    findall(A, precond(A), Precond).

%Precond is a list of tuple of Safe and Unsafe approximations, assume that Pecond is not empty
%eg Precond = [([[a], [b]], [[c], [d]])]

getSuffPrecond([(S, U)], Safe, Unsafe, SafeYices, UnsafeYices, Vs):-
    !,
    simplify_dnf_2_dnf(S, Vs, S1),
    simplify_dnf_2_dnf(U, Vs, U1),
    (
(( (S1=[[true]], U1=[[true]]); (S1=[[false]], U1=[[false]]))-> Safe=[[false]], Unsafe=[[false]],
        SafeYices=(false), UnsafeYices=(false))
    ;

    (S1=[[true]], U1=[[false]]-> Safe=[[true]], Unsafe=[[false]],
        SafeYices=(true), UnsafeYices=(false))
    ;
    ((S1=[[false]]), U1=[[true]]-> Safe=[[false]], Unsafe=[[true]],
        SafeYices=(false), UnsafeYices=(true))
    ;
    (S1=[[false]] -> Safe=[[false]], Unsafe= U1,
            SafeYices=(false), listofList2YicesDisj(U1, UnsafeYices))
    ;
    ((U1=[[false]]) -> Unsafe=[[false]], Safe= S1,
                    UnsafeYices=(false), listofList2YicesDisj(S1, SafeYices))
    ;
    (S1=[[true]] -> Safe= not(U1), Unsafe= [[false]],
        UnsafeYices=(false), listofList2YicesDisj(U1, SafeYices1), SafeYices= neg(SafeYices1))
    ;
    (U1=[[true]] -> Unsafe= not(S1), Safe= [[false]],
        SafeYices=(false), listofList2YicesDisj(S1, UnsafeYices1), UnsafeYices= neg(UnsafeYices1))
    ;
      %(optimal-> Safe=S1, Unsafe=U1 %this applies only to the iteration in which optimality was detected
      %;
       Safe=(S1, not(U1)), Unsafe=(U1, not(S1)),
      %),
      listofList2YicesDisj(S1, S1Yices),
      listofList2YicesDisj(U1, U1Yices),
      UnsafeYices= (U1Yices, neg(S1Yices)),
      SafeYices= (S1Yices, neg(U1Yices))
).

getSuffPrecond([H|L], Safe, Unsafe, SYices, UYices, Vs):-
    !,
    getSuffPrecond([H], S, U, SafeYices, UnsafeYices, Vs),
    %write('safe '), write(S), write(' unsafe '), write(U), nl,
    ((S=[false], U=[false]) -> Safe= S1, SYices=SafeYices1, Unsafe= U1, UYices=UnsafeYices1
    ;
        S=[false]  -> Safe= S1, SYices=SafeYices1, Unsafe= (U;U1), UYices=(UnsafeYices;UnsafeYices1)
    ;
        U=[false] -> Safe= (S;S1), SYices=(SafeYices;SafeYices1), Unsafe= U1, UYices=UnsafeYices1
    ;
        Safe= (S;S1), SYices=(SafeYices;SafeYices1), Unsafe= (U;U1), UYices=(UnsafeYices;UnsafeYices1)
    ),
    getSuffPrecond(L, S1, U1,  SafeYices1, UnsafeYices1,Vs).
getSuffPrecond([], [false], [false],  (false), (false),_). %default.


% ---------------------------------------------------------------------------
% clause splitting
% ---------------------------------------------------------------------------

split_clauses(F, FOut):-
    clauseSplitting:main(['-prg', F, '-o', FOut]).


% ---------------------------------------------------------------------------
% main procedure analyse_non_term BEGIN
% ---------------------------------------------------------------------------

analyse_non_term(Prog2, WithInterpolant,Bounded) :-

    logfile_non_term(LogFile),
	%open(LogFile, append, LogS),
    open(LogFile, write, LogS),
    %write(LogS, 'processing file: '), write(LogS, Prog2), nl(LogS),
    prepare_resultdir_non-term(Prog2, ResultDir),

    verbose_message(['temp files dir ', ResultDir]),
    path_basename(Prog2, F),
    createTmpFileNonTerm(ResultDir, F, F_Verify_Loop_Exit, F_Verify_Loop_Entry, F_Split),
    verbose_message(['temp files dir nt ', F_Verify_Loop_Entry]),
    (flag(clssplit)->
        verbose_message(['Clause splitting transformation']),
        split_clauses(Prog2, F_Split),
        Prog1=F_Split
    ;
        Prog1=Prog2
    ),
    load_file(Prog1),
    %TODO: optimise, only needed for init state vars
    record_initial_states(Prog1,  _, InitVars),
    dependency_graph(Es,Vs),
    verbose_message(['===================dep graph================']),
    verbose_message([Es]),  verbose_message([Vs]), nl,
    scc_graph(Es,Vs,SCCs, BEs), %in topological order
    verbose_message(['===================SCC================']),
    verbose_message([SCCs]),
    verbose_message(['===================Backedges================']),
    verbose_message([BEs]),
	%naturalLoops(Es,Vs,NLoops),
    loopHeaders_frm_bck_edges(BEs, LHeaders),
    %loopHeaders(NLoops, LHeaders),
    verbose_message(['loop headers ',LHeaders]),
    ChunkNo=0,
    statistics(runtime,[START|_]),
    divide_analyse(Prog1, Es, SCCs, LHeaders, F_Verify_Loop_Exit,F_Verify_Loop_Entry, WithInterpolant,Bounded, ChunkNo, PrecondNonTerm, InitVars),
    classify_non_term(PrecondNonTerm, InitVars, Status),
    statistics(runtime,[END|_]),
    DIFF is END - START,
    write('=================precond non-termination==============='), nl,
    (Status=unsat ->
    printNonTermOutput(LogS,F, 'may-terminating', WithInterpolant, DIFF);
    printNonTermOutput(LogS,F, 'non-terminating', WithInterpolant, DIFF)
    ),
    Init=..[init|InitVars],
    %write(LogS, Init), write(LogS, ' <-- '), write(LogS, PrecondNonTerm), nl(LogS),
    write(Init), write( ' <-- '), write(PrecondNonTerm), nl,
    close(LogS),
    end_resultdir(ResultDir).

loopHeaders_frm_bck_edges([], []).
loopHeaders_frm_bck_edges([_-B|R], [B|Rh]):-
    loopHeaders_frm_bck_edges(R, Rh).

loopHeaders([], []).
loopHeaders([(L, _)|Loops], [L|Ls]):-
    loopHeaders(Loops,Ls).

classify_non_term(PrecondNonTerm, InitVars, Status):-
    listofList2YicesDisj(PrecondNonTerm, PrecondNonTermYices),
    yices_init,
    check_unsat_w_res(PrecondNonTermYices, InitVars, Status),
    yices_exit.


%PrecondNonTerms: the second last arg. reps disjunctions
divide_analyse(_,_, [],_, _,_,_,_,_, [false], _).
divide_analyse(Prog1,Es, SCCs,LHeaders, IntermedFile1,IntermedFile2,WithInterpolant,Bounded, ChunkNo, PrecondNonTerms, InitVars):-
    verbose_message(['making scc division']),
    split_sccs(SCCs,Scc_current, Scc_future), %Scc_current will get processed this time while the Scc_future gets processed later
    (scc_contain_rec_component(Scc_current) ->
    gen_cls_frm_scc(Scc_current, LHeaders, Es, ClsIds, EntryCls, ExitCls),
    verbose_message(['entry cls ', EntryCls]),
    get_cls_frm_ids(ClsIds, Clauses),
    append(Clauses,EntryCls, PInter),
    append(PInter, ExitCls, Prog),
    (ChunkNo=1 -> %remove false and safe clauses
        remove_safe_false_cls(IntermedFile1); true
    ),
    writeClsListToFile(Prog, IntermedFile1), %will append to the previous
    verbose_message(['=============applying pi-horn=========================']),
    %clear traces results before applying pi_horn
    cleanup_trace_constr,
    retractall_fact(optimal), % optimality applies to each sccs we considered
    apply_PI_Horn(IntermedFile1, WithInterpolant,Bounded, SuffSafePrecond),
    nl, nl, nl,
    write('=======================suff precond ====================='), nl,
    write(SuffSafePrecond), nl,
%test whether a loop entry is reachable, TODO:  better

    (SuffSafePrecond=[false];SuffSafePrecond=false->PrecondNonTerm=[false];
        %for each dnf, need to check if loop entry is reachable, then from that dnf the recurrent set is reachable
        verbose_message(['checking init reachability from init state ']), verbose_message([SuffSafePrecond]),
        check_reachability_loop_entry(InitVars, SuffSafePrecond, IntermedFile1, IntermedFile2, PrecondNonTerm)
    ),
    load_file(Prog1),
    divide_analyse(Prog1,Es, Scc_future,LHeaders, IntermedFile1,IntermedFile2, WithInterpolant,Bounded, 1, PrecondNonTerm1, InitVars),
    ((PrecondNonTerm1=[false]; PrecondNonTerm1=false) -> PrecondNonTerms=PrecondNonTerm;
      (PrecondNonTerm=[false]; PrecondNonTerm=false) -> PrecondNonTerms=PrecondNonTerm1;
      append(PrecondNonTerm, PrecondNonTerm1, PrecondNonTerms)
    )
    ;
        PrecondNonTerms=[[false]] %case when there is no loop in the considered scc
    ).

check_reachability_loop_entry(InitVars, (InitConstr;InitConstrs), IntermedFile1, IntermedFile2, Precond):-
    !,
    verbose_message(['init constr seq ', InitConstr]),
    yices_init,
    precond_patt_2_dnf(InitConstr, InitVars, DNF),
    %write(InitConstr), write(' DNF pattern============================ '), write(DNF), nl,
    yices_exit,
    % creates a program for checking reachability
    create_solve_recurrent_rechability_prg(DNF, IntermedFile1, IntermedFile2, PNT),
    check_reachability_loop_entry(InitVars, InitConstrs, IntermedFile1, IntermedFile2, PrecondNonTerms),
    append(PNT,PrecondNonTerms,Precond).
check_reachability_loop_entry(_, [[false]], _, _, [[false]]):-
    !.
check_reachability_loop_entry(InitVars, InitConstr, IntermedFile1, IntermedFile2, PNT):-
    verbose_message(['init constr single ', InitConstr]),
    !,
    yices_init,
    precond_patt_2_dnf(InitConstr, InitVars, DNF),
    yices_exit,
    (DNF=[[false]] -> PNT=[[false]]
    ;
     % creates a program for checking reachability
     write('reachability called for ====== '), write(DNF), nl,
     create_solve_recurrent_rechability_prg(DNF, IntermedFile1, IntermedFile2, PNT)
    ).



reachable_loop_entry(InputFile, Res):-
    %bring rahft here
    verbose_message(['calling rahft for reachability check']),
    %TODO: make sure result.txt is not already there, since it will get appended
    RFile= 'result.txt', %assume that rahft outputs its result in result.txt
    (file_exists(RFile) -> process_call(path('rm'), [RFile],[])
	  ; true
    ),
    process_call(path('rahft'), [InputFile],[]), %will store results in result.txt
    open(RFile, read, S),
    reachability_res(S, Res),
    close(S),
    (file_exists(RFile) -> process_call(path('rm'), [RFile],[])
	  ; true
    ),
    verbose_message(['rahft call succeeded!']).

reachability_res(end_of_file, _):- write('verification ended with failure!!!!!'), halt(1).
reachability_res(S, Res):-
    read(S,ResultList),
    !,
    get_res(ResultList, Res).

get_res([], _).
get_res([safety(Res)|_], Res):-
    !.
get_res([_|L], Res):-
    !,
    get_res(L, Res).

replace_safe_by_false:-
    my_clause(safe, B, C),
    retract_fact(my_clause(safe, B, C)),
    assertz_fact(my_clause(false, B, C)),
    fail.
replace_safe_by_false.


create_solve_recurrent_rechability_prg([false], _, _, [false]):-
    !.
create_solve_recurrent_rechability_prg([InitConstr], IntermedFile1, IntermedFile2, [PNT]):-
    !,
    write('Calling rahft for entries======= '), write(InitConstr), nl,
    verbose_message(['init constr from dnf ',InitConstr]), nl,
    load_file(IntermedFile1),
    retractall_fact(my_clause(false, _, _)),
   % (my_clause(safe, B, C), retractall_fact(my_clause(safe, B, C)) -> %assuming single safe clause
    %assertz_fact(my_clause(false, B, C))), %changing safe to false
    replace_safe_by_false,
    %TODO make it work for all variants initXXX
    my_clause(Init,_,_),
    Init=..[init|_],
    retractall_fact(my_clause(Init, _, _)),
    assertz_fact(my_clause(Init,InitConstr,0)),
    dumpClstoFile(IntermedFile2),
    reachable_loop_entry(IntermedFile2, Res),
    (Res=unsafe -> %Res=safe means unreachable recurrent set.
    PNT=InitConstr;
    PNT=[false]).

create_solve_recurrent_rechability_prg([InitConstr|Constrs], IntermedFile1, IntermedFile2, [PNT|Precond]):-
    !,
    verbose_message(['init constr from dnf chain ',InitConstr]),
    load_file(IntermedFile1),
    retractall_fact(my_clause(false, _, _)),
    (my_clause(safe, B, C), retractall_fact(my_clause(safe, B, C))->         assertz_fact(my_clause(false, B, C))), %changing safe to false

    %TODO make it work for all variants initXXX
    my_clause(Init,_,_),
    Init=..[init|_],
    retractall_fact(my_clause(Init, _, _)),
    assertz_fact(my_clause(Init,InitConstr,0)),
    dumpClstoFile(IntermedFile2),
    reachable_loop_entry(IntermedFile2, Res),
    (Res=unsafe -> %Res=safe means unreachable recurrent set.
    PNT=InitConstr;
    PNT=[false]),
    create_solve_recurrent_rechability_prg(Constrs, IntermedFile1, IntermedFile2, Precond).
create_recurrent_rechability_prg([], _, _, [[false]]):-
    write('No initial states given'), write(_), nl.


remove_safe_false_cls(File):-
    load_file(File),
    retractall_fact(my_clause(safe, _, _)),
    retractall_fact(my_clause(false, _, _)),
    %write rest to the File
    dumpClstoFile(File).

get_cls_frm_ids([], []).
get_cls_frm_ids([Id| Ids], [Cl|Cls]):-
    my_clause(H, B, Id),
    Cl= (H, B),
    get_cls_frm_ids(Ids, Cls).

gen_cls_frm_scc([],_, _, [], [], []).
%clauses with head in SCC are enough
gen_cls_frm_scc([(non_recursive, SCC)| SCCs],LHeaders, Es, ClsIds, EntryCls, ExitCls):-
    findall(Id, (my_clause(H, _, Id), functor(H, P, N), member(P/N, SCC)), Ids),
    gen_cls_frm_scc(SCCs,LHeaders, Es, Rest, EntryCls, ExitCls),
    append(Ids, Rest, ClsIds).
%when it comes to the recursive one, split makes sure that it is the last one to process
gen_cls_frm_scc([(recursive, SCC)| _],LHeaders, Es, ClsIds, EntryCls, ExitCls):-
    findall(Id, (my_clause(H, _, Id), functor(H, P, N), member(P/N, SCC)), ClsIds),
    %no recursion needed as it is the last one; generate entry and exit cls
    %%TODO: SCC head is not a loophead, fix for nested loop
    get_loop_head(SCC, LHeaders, SCCHead),
    verbose_message(['SCCHead ', SCCHead]),
    %findall(R, (member(SCCHead-R, Es), \+ member(R, SCC)), SCCEntries),
    findall(K-R, (member(K, SCC), member(K-R, Es), \+ member(R, SCC)), SCCEntries),
    %findall(Q, (member(Q-SCCHead, Es), \+ member(Q, SCC)), SCCExits),
    findall(Q-X, (member(X, SCC),member(Q-X, Es), \+ member(Q, SCC)), SCCExits),
    verbose_message(['entries ', SCCEntries]),
    verbose_message(['exits ']), verbose_message([SCCExits]),
    gen_scc_entry_cls( SCCEntries, EntryCls),
    verbose_message(['entry cls generated']),
    gen_scc_exit_cls( SCCExits, ExitCls),
    verbose_message(['exit cls generated']).

%assume scc and LHeaders is non-empty
get_loop_head(SCC, LHeaders, A):-
    member(A, SCC),
    member(A, LHeaders),
    !.

construct_entry_cls_pattern([], []).
construct_entry_cls_pattern([Id|Ids], [Cl|Cls]):-
    my_clause(_H, B, Id),
    separate_constraints(B,  Cs, Bs),
    varset(Cs, CsVar),
    varset(Bs, BodyAtomsVars),
    setdiff(CsVar, BodyAtomsVars, RmVars),
    numbervars((BodyAtomsVars, CsVar), 0, _),
    start_ppl,
    removeExistsVars(Cs, RmVars, Cs1), %need to remove vars not in the body predicates
    end_ppl,
    append(Cs1, Bs, Bs1),
    Cl=(safe, Bs1),
    construct_entry_cls_pattern(Ids, Cls).

gen_scc_entry_cls( [], []).
gen_scc_entry_cls( [EP/EN-BP/BN|Entries], Clauses):-
    findall(Id,
            (functor(HE, EP, EN),
            functor(BE, BP, BN),
            my_clause(HE, B, Id),
            separate_constraints(B, _Cs, Bs),
            member(BE, Bs)
            ),
            Ids
           ),
    construct_entry_cls_pattern(Ids, Cls),
    gen_scc_entry_cls(Entries, ClsRest),
    append(Cls, ClsRest, Clauses).


construct_exit_cls_pattern([], []).
construct_exit_cls_pattern([Id|Ids], [Cl|Cls]):-
    my_clause(_H, B, Id),
    separate_constraints(B,  Cs, Bs),
    varset(Bs, VsBs),
    varset(Cs, CsVar),
    setdiff(CsVar, VsBs, RmVars),
    numbervars((VsBs, CsVar), 0, _),
    start_ppl,
    removeExistsVars(Cs, RmVars, Cs1),
    end_ppl,
    append(Cs1, Bs, Bs1),
    Cl=(false, Bs1),
    construct_exit_cls_pattern(Ids, Cls).

gen_scc_exit_cls([], []).
gen_scc_exit_cls( [EP/EN-BP/BN|Exits], Clauses):-
    findall(Id,
            (functor(HE, EP, EN),
            functor(BE, BP, BN),
            my_clause(HE, B, Id),
            separate_constraints(B, _Cs, Bs),
            member(BE, Bs)
            ),
            Ids
           ),
    construct_exit_cls_pattern(Ids, Cls),
    gen_scc_exit_cls(Exits, ClsRest),
    append(Cls, ClsRest, Clauses).

/*
gen_scc_exit_cls( [EP/EN-BP/BN|Exits], [Cl| Cls]):-
    functor(HE, EP, EN),
    %write(HE), nl,
    functor(BE, BP, BN),
    my_clause(HE, B, _),
    separate_constraints(B,  Cs, Bs),
    member(BE, Bs),
    varset(Bs, VsBs),
    varset(Cs, CsVar),
    setdiff(CsVar, VsBs, RmVars),
    numbervars((VsBs, CsVar), 0, _),
    start_ppl,
    removeExistsVars(Cs, RmVars, Cs1),
    end_ppl,
    append(Cs1, Bs, Bs1),
    Cl=(false, Bs1),
    gen_scc_exit_cls(Exits, Cls).
*/



recursive_scc_cls([], _, _, []).
recursive_scc_cls([P/N | Loop], OrigLoop, Es, Clauses):-
    functor(H, P, N),
    collect_scc_cls_ids(H, OrigLoop, Cs),
    recursive_scc_cls(Loop, OrigLoop, Es, Cls),
    append(Cs, Cls, Clauses).

collect_scc_cls_ids(Head, Loop, Ids):-
    findall(C,
    (my_clause(Head, B, C),
    separate_constraints(B, _, Bs),
    %one of Bs in the loop
    (Bs=[] -> true %include constrained fact
    ;
        atleast1inscc(Bs, Loop)
    )),
    Ids).

%at least one in the loop, assume the first arg is a non-empty list
atleast1inscc([B|_], OrigLoop):-
    functor(B, P, N),
    member(P/N, OrigLoop), !.
atleast1inscc([_|Bs], OrigLoop):-
    !,
    atleast1inscc(Bs, OrigLoop).
atleast1inscc([], _):- fail.



    %analyse_loops_individually(NLoops, Es).
    %extract_loop_programs(NLoops, Es, Ps).

%splits a given set of sccs in topological order into two, upto the first recursive and the rest
split_sccs([], [], []).
split_sccs([(non_recursive, SCC)| SCCs], [(non_recursive, SCC)|Analysed], Unanalysed):-
    !,
    split_sccs(SCCs, Analysed, Unanalysed).
split_sccs([(recursive, SCC)| SCCs], [(recursive, SCC)|_], SCCs).

%contains if the input scc contains a recursive component

scc_contain_rec_component([(recursive, _)| _]):-
    !.
scc_contain_rec_component([(non_recursive, _)| SCCs]):-
    !,
    scc_contain_rec_component(SCCs).
scc_contain_rec_component([]):- fail.


%extract_prog_to_analyse([], _, [], []).
%extract_prog_to_analyse([(non-recursive, SCC)| SCCs], LHeads, [SCC|Analysed], Unanalysed).


apply_PI_Horn(Prog1, WithInterpolant,Bounded, SimplifiedSafePrecond) :-
    %check presence of both safe and false predicates, only relevant for precond
    %generation

    initialise_tr_constrs,
    record_initial_states(Prog1,  InitStates, InitVars),
    verbose_message(['processing file: ']), verbose_message([Prog1]),
    verbose_message(['Init constrs: ']), verbose_message([InitStates]),
    assertz_fact(mixState(InitStates)),
    verbose_message(['Init vars: ']), verbose_message([InitVars]),
	logfile_pi_horn(LogFile),
	open(LogFile, append, LogS),
    %write(LogS, 'processing file: '), write(LogS, Prog1), nl(LogS),
	%creating temporary directory for intermediate files
	prepare_resultdir(Prog1, ResultDir),
    prepare_safe_resultdir(Prog1, ResultDirSafe),
    verbose_message(['temp files dir ', ResultDir]),
	K = 0,
	path_basename(Prog1, F),
	createTmpFilePP(ResultDir, F, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD),
	createTmpFileRef(ResultDir, F, F_FTA, F_DFTA, F_SPLIT, F_REFINE),
    safe_file(ResultDirSafe, F, F_SAFE), %creates empty file F_SAFE
    %F_SAFE contains all cls of Prog1 except false:- and safe:- replaced with false:-
    %TODO: safe replaced with false (which is not the ideal way)
    replaceSafePred(Prog1, F_SAFE),
    %for clauses with safe predicates
    createTmpFilePP(ResultDirSafe, F, F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE),
	createTmpFileRef(ResultDirSafe, F, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE),

	statistics(runtime,[START|_]),
    verbose_message(['abs ref BEGIN!']),
	abstract_refine1(InitVars,Bounded, LogS,  Prog1, K, Result, K1, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE, F_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE),
	statistics(runtime,[END|_]),
    DIFF is END - START,
    % printing statistics
    path_basename(Prog1, F),
    %printing results
    verbose_message(['abs ref DONE!']),
    display_results(F, InitVars, WithInterpolant, F_REFINE, F_REFINE_SAFE, Result, Result_SAFE,K1, DIFF, LogS, ResultDir, ResultDirSafe, SimplifiedSafePrecond).

% ---------------------------------------------------------------------------
% main procedure apply_PI_Horn END
% ---------------------------------------------------------------------------



%%%%%%%%%%%%%%%% Loops %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loops([(Header,NLoop)|NLoops],Graph,Loops) :-
	loops(NLoops,Graph,Loops2),
	(setof(Path, (
			loopPath(Header,NLoop,Graph,[],Path)),
			Loops1) -> true; Loops1=[]),
	append(Loops1,Loops2,Loops).
loops([],_,[]).

loopPath(P,NLoop,Graph,As,Path) :-
	path(P,P,Graph,NLoop,As,Path).
	
path(P,R,Graph,NLoop,As,[P|Path]) :-
	member(P-Q,Graph),
	member(Q,NLoop),
	\+ member(Q,As),
	pathContinue(Q,R,Graph,NLoop,[Q|As],Path).
	
pathContinue(R,R,_,_,_,[R]).
pathContinue(Q,R,Graph,NLoop,As,Path) :-
	Q \== R,
	path(Q,R,Graph,NLoop,As,Path).

showLoops(Ns) :-
	write('Loops'),nl,
	showLoopList(Ns).
	
showLoopList([]) :-
	nl,nl.
showLoopList([N|Ns]) :-
	write(N),
	nl,
	showLoopList(Ns).

% ---------------------------------------------------------------------------
%displaying the results of preconditions with time statistics
% ---------------------------------------------------------------------------


display_results(F, InitVars, WithInterpolant, F_REFINE, F_REFINE_SAFE, Result, Result_SAFE,  K, DIFF, LogS, _ResultDir, _ResultDirSafe, SimplifiedSafePrecond):-
    extract_nec_precond(Result, Result_SAFE, F_REFINE, F_REFINE_SAFE, InitVars, Precond),
    verbose_message(['nec precond from clauses ', Precond]),
    write('nec precond from clauses ================== '), nl, write(Precond),nl,
    compute_suff_precond(Precond, Safe, Unsafe, SafeYices, UnsafeYices, InitVars),
    simplify_pretty_print_res(F, LogS, InitVars, Safe, SafeYices, Unsafe, UnsafeYices, K, WithInterpolant, DIFF, SimplifiedSafePrecond).

%TODO: fix the following delete file line,
    %delete_temp_dirs(ResultDir, ResultDirSafe).


extract_nec_precond(Result, Result_SAFE, F_REFINE, F_REFINE_SAFE, InitVars, Precond):-
    %write('extracting nec precond =========='), nl,
     yices_init,
    (optimal ->
        getPrecond(Precond),
        verbose_message(['Opt precond (SAFE, UNSAFE) ']),  verbose_message([Precond])
;
    (
    (nonvar(Result), nonvar(Result_SAFE))->  Cs=[[false]], Cs_SAFE=[[false]]
    ; nonvar(Result) -> Cs=[[false]], checkFalseInFile:checkForFalse(F_REFINE_SAFE, Res_SAFE),
            (Res_SAFE=safe -> % no (safe :- ...)
                Cs_SAFE=[[false]]
            ;
                getInitCondFile(F_REFINE_SAFE, Cs_SAFE)
            )
    ; nonvar(Result_SAFE) -> Cs_SAFE=[[false]],  checkFalseInFile:checkForFalse(F_REFINE, Res),
            (Res=safe -> % no (false :- ...)
                Cs=[[false]]
            ;
                getInitCondFile(F_REFINE, Cs)
            )
    ; %default both are not safe
        checkFalseInFile:checkForFalse(F_REFINE, Res),
            (Res=safe -> % no (false :- ...)
                Cs=[[false]]
            ;
                getInitCondFile(F_REFINE, Cs)
            ),
        checkFalseInFile:checkForFalse(F_REFINE_SAFE, Res_SAFE),
            (Res_SAFE=safe -> % no (safe :- ...)
                Cs_SAFE=[[false]]
            ;
                getInitCondFile(F_REFINE_SAFE, Cs_SAFE)
            )
    ),
    verbose_message(['residual prog. constr unsafe: ']), verbose_message([Cs]),
    verbose_message(['residual prog. constr safe: ']), verbose_message([Cs_SAFE]),
    verbose_message(['init vars ', InitVars]),
    combine_w_trace_constrs(Cs, Cs_SAFE, InitVars, Cs3, Cs3_SAFE),
    verbose_message([' nec unsafe: ']), verbose_message([Cs3]),
    verbose_message(['nec safe: ']), verbose_message([Cs3_SAFE]),
    %write('getting precond======================='), nl,
    getPrecond(Precond1),
    %write('precond1 ' ), nl, write(Precond1), nl,

    %this is needed if one of prog was proven safe and refinement loop is exited
    (unsatApproxSafeUnsafe(Cs3_SAFE, Cs3,InitVars)->
        % optimal precond

        assertz_fact(optimal)
    ;
        true
    ),
    (Cs3_SAFE==Cs3 ->
        Precond=Precond1 %syntactic check
    ;
        append([(Cs3_SAFE, Cs3)], Precond1, Precond)
    ),
    verbose_message(['precond (SAFE, UNSAFE)']),  verbose_message([Precond])
).

combine_w_trace_constrs(Cs, Cs_SAFE, InitVars, Cs3, Cs3_SAFE):-
    %Cs3 is the list of list of unsafe states
     %write('combining unsafe trace constraints with '), write( Cs), nl,
    combineTraceConstraints(unsafeCls, Cs, Cs3, InitVars),
    %write('combining safe trace constraints with '), write( Cs_SAFE), nl,
    %Cs3_SAFE is the list of list of safe states
    combineTraceConstraints(safeCls, Cs_SAFE, Cs3_SAFE, InitVars).


compute_suff_precond(Precond, Safe, Unsafe, SafeYices, UnsafeYices, InitVars):-
    yices_init,
    verbose_message(['precond == ', Precond]),
    getSuffPrecond(Precond, Safe, Unsafe, SafeYices, UnsafeYices, InitVars),
    verbose_message(['suff precond SAFE']), verbose_message([Safe]),
    verbose_message(['suff precond UNSAFE']),  verbose_message([Unsafe]).

simplify_pretty_print_res(F, LogS, InitVars, Safe, SafeYices, Unsafe, UnsafeYices, K1, WithInterpolant, DIFF, SimplifiedSafePrecond):-
    InitPred=..[init|InitVars],
    verbose_message(['====================== Suff Cond ===========================']), nl,
    (optimal->
        %the nec and suff precond are the same
        printRahftOutput(LogS,F, 'safe-optimal','unsafe-optimal', K1, WithInterpolant, DIFF),
        %print to log file
        write(LogS,'safe:'),nl(LogS),
        write(LogS,InitPred), write(LogS,'<-- '), write(LogS,Safe), nl(LogS),
        write(LogS,'unsafe:'),nl(LogS),
        write(LogS,InitPred), write(LogS,'<-- '), write(LogS,Unsafe), nl(LogS),
        %print to console
        write('safe:'),nl,
        write(InitPred), write('<-- '), write(Safe), nl,
        SimplifiedSafePrecond=Safe,
        write('unsafe:'),nl,
        write(InitPred), write('<-- '), write(Unsafe), nl
    ;
        check_unsat_w_res(SafeYices, InitVars, SatResSafe),
        write(SatResSafe), nl,
        check_unsat_w_res(UnsafeYices, InitVars, SatResUnsafe),
        write(SatResSafe), nl, write(SatResUnsafe), nl,
        ((SatResSafe=unsat, SatResUnsafe=unsat)->
                %both precond are trivial
                printRahftOutput(LogS,F, 'safe-trivial','unsafe-trivial', K1, WithInterpolant, DIFF),
                %print to log file
                write(LogS,'safe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,'false'), nl(LogS),
                write(LogS,'unsafe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,'false'), nl(LogS),
                %print to console
                write('safe:'),nl,
                write(InitPred), write('<-- '), write('false'), nl,
                SimplifiedSafePrecond=false,
                write('unsafe:'),nl,
                write(InitPred), write('<-- '), write('false'), nl
        ; SatResSafe=unsat->
                %safe-trivial, unsafe non-trivial
                printRahftOutput(LogS,F, 'safe-trivial','unsafe-non-trivial', K1, WithInterpolant, DIFF),
                %print to log file
                write(LogS,'safe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,'false'), nl(LogS),
                write(LogS,'unsafe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,Unsafe),  nl(LogS),
                %print to console
                write('safe:'),nl,
                write(InitPred), write('<-- '), write('false'), nl,
                SimplifiedSafePrecond=false,
                write('unsafe:'),nl,
                write(InitPred), write('<-- '), write(Unsafe),  nl
        ; SatResUnsafe=unsat->
                %safe-non-trivial, unsafe trivial
                printRahftOutput(LogS,F, 'safe-non-trivial','unsafe-trivial', K1, WithInterpolant, DIFF),
                %print to log file
                write(LogS,'safe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,Safe),  nl(LogS),
                write(LogS,'unsafe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,'false'), nl(LogS),
                %print to console
                write('safe:'),nl,
                write(InitPred), write('<-- '), write(Safe), nl,
                SimplifiedSafePrecond=Safe,
                write('unsafe:'),nl,
                write(InitPred), write('<-- '), write('false'), nl
        ; %both non-trivial
                printRahftOutput(LogS,F, 'safe-non-trivial','unsafe-non-trivial', K1, WithInterpolant, DIFF),
                %print to log file
                write(LogS,'safe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,Safe),  nl(LogS),
                write(LogS,'unsafe:'),nl(LogS),
                write(LogS,InitPred), write(LogS,'<-- '), write(LogS,Unsafe),  nl(LogS),
                %print to console
                write('safe:'),nl,
                write(InitPred), write('<-- '), write(Safe),  nl,
                SimplifiedSafePrecond=Safe,
                write('unsafe:'),nl,
                write(InitPred), write('<-- '), write(Unsafe), nl
        )

    ),
    write('============================================================'), nl,
    yices_exit.

delete_temp_dirs(ResultDir, ResultDirSafe):-
    end_resultdir(ResultDir),
    end_resultdir(ResultDirSafe).


derive_nec_precond(Prog, Res, Prog_SAFE, Res_SAFE, Cs3, Cs3_SAFE, InitVars):-
    checkFalseInFile:checkForFalse(Prog, Res),
    (Res=safe -> % no (false :- ...)
        Cs=[[false]]
    ;
        %write('getting precond unsafe'), write(Prog),nl,
        getInitCondFile(Prog, Cs)
    ),
    checkFalseInFile:checkForFalse(Prog_SAFE, Res_SAFE),
    (Res_SAFE=safe -> % no (safe :- ...)
        Cs_SAFE=[[false]]
    ;
        %write('getting precond safe'), nl,
        getInitCondFile(Prog_SAFE, Cs_SAFE)
    ),
    %write('obtaining traces'), nl,
    combine_w_trace_constrs(Cs, Cs_SAFE, InitVars, Cs3, Cs3_SAFE).

simplify_nec_precond(Cs3, Cs3_SAFE, InitVars, Cs31, Cs3_SAFE1):-
    yices_init,
    check_tautology_w_res(Cs3_SAFE, InitVars, Taut_Cs3_SAFE),
    check_tautology_w_res(Cs3, InitVars, Taut_Cs3),
    (Taut_Cs3_SAFE=tautology, Taut_Cs3=tautology ->
       % write('tautology detected in both cases===================='), nl,
        Cs3_SAFE1=[[true]], Cs31=[[true]]
    ;  Taut_Cs3_SAFE=tautology ->
        Cs3_SAFE1=[[true]], simplify_dnf_2_dnf(Cs3, InitVars, Cs31)
    ;  Taut_Cs3=tautology ->
        Cs31=[[true]], simplify_dnf_2_dnf(Cs3_SAFE, InitVars, Cs3_SAFE1)
    ;
        simplify_dnf_2_dnf(Cs3, InitVars, Cs31),
        simplify_dnf_2_dnf(Cs3_SAFE, InitVars, Cs3_SAFE1)
    ),
    yices_exit.

reset_trConstr_facts:-
    retractall_fact(trConstr(_)),
    retractall_fact(trConstrSafe(_)),
    retractall_fact(trConstrAcc(_)),
    retractall_fact(trConstrSafeAcc(_)).


derive_simplify_nec_precond(Prog1, Res, Prog_SAFE, Res_SAFE, Cs31, Cs3_SAFE1, InitVars):-
    derive_nec_precond(Prog1, Res, Prog_SAFE, Res_SAFE, Cs3, Cs3_SAFE, InitVars),
    verbose_message(['derived nec precond']),
    verbose_message([Cs3_SAFE]),
    simplify_nec_precond(Cs3, Cs3_SAFE, InitVars, Cs31, Cs3_SAFE1),
    verbose_message(['derived suff precond']),
    verbose_message([Cs3_SAFE1]).


%===============abstract_refine with iteration count begin===================

abstract_refine1(InitVars, Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,Prog_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE):-
    K=0,
    !,
    %write('calling abstract_refine'), nl,
    abstract_refine(InitVars, Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,Prog_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE).

abstract_refine1(InitVars, Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,Prog_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE):-
    %K>0, not needed due to cut above
    derive_simplify_nec_precond(Prog1, Tmp_Res, Prog_SAFE, Tmp_Res_SAFE, Approx_unsafe, Approx_safe, InitVars),
    abstract_refine2(InitVars, Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,Prog_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE, Tmp_Res, Tmp_Res_SAFE, Approx_unsafe, Approx_safe).

%intersection of safe and unsafe states is empty =>optimality
abstract_refine2(InitVars, _, _,  _, K, _, K2, _, _, _, _, _, _,_, _, _, _, _, _, _,_,  _,   _, _, _, _, _,_, _, _, _, _, _, _, _,_, Cs31, Cs3_SAFE1):-
        unsatApproxSafeUnsafe(Cs3_SAFE1, Cs31,InitVars),
        !,
        %write(Cs31), nl, write(Cs3_SAFE1), nl,
        write('========Optimality detected================ '), nl,
        % optimal precond
        assertz_fact(optimal),
        cond_assert_precond(Cs3_SAFE1, Cs31),
        K2=K. %setting the number of iterations

%one of the program is safe
abstract_refine2(_, _, _,  _, K, _, K2, _, _, _, _, _, _,_, _, _, _, _, _, _,_,  _,   _, _, _, _, _,_, _, _, _, _, _, _, Res, Res_SAFE, Cs31, Cs3_SAFE1):-
        %derive_simplify_nec_precond(Prog1, Res, Prog_SAFE, Res_SAFE, Cs31, Cs3_SAFE1, InitVars),
        %abstract_refine if none of Res or Res_SAFE are safe
        (Res=safe; Res_SAFE=safe),
        !,
        verbose_message(['========One of the Program is safe================ ']),
        cond_assert_precond(Cs3_SAFE1, Cs31),
        verbose_message(['(safe, unsafe) precond ', (Cs3_SAFE1, Cs31)]),
        K2=K.

%the intersection of safe and unsafe states is not stronger than the previous one
abstract_refine2(InitVars, _, _,  _, K, _, K2, _, _, _, _, _, _,_, _, _, _, _, _, _,_,  _,   _, _, _, _, _,_, _, _, _, _, _, _, _,_, Cs31, Cs3_SAFE1):-
        %derive_simplify_nec_precond(Prog1, _, Prog_SAFE, _, Cs31, Cs3_SAFE1, InitVars),
        %write('Forming new init state'), nl,
        %length(Cs31, L1),length(Cs3_SAFE1, L2),
        %write('unsafe form size: '), write(L1), nl,
        %write('safe form size: '), write(L2), nl,
        %write('first '), write(Cs31), nl,
        write('second safe '), write(Cs3_SAFE1), nl,
        % past implies the current => no improvement is made. STOP
        retract(mixState(MixConstr)), %retract it, we need to replace it by the current one
        yices_init,
        %write('implication check '), nl,
        implies(MixConstr, (Cs31, Cs3_SAFE1), InitVars),
        !,
        verbose_message(['=========the current intersection does not get stronger=======']),
        yices_exit,
        %reset_trConstr_facts,
        cond_assert_precond(Cs3_SAFE1, Cs31),
        %current approximation does not change, stop
        K2=K.

%the intersection of safe and unsafe states is not empty, start with the intersection
abstract_refine2(InitVars, Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,Prog_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE, _,_, Cs31, Cs3_SAFE1):-
        !,
        cond_assert_precond(Cs3_SAFE1, Cs31),
        retractall_fact(mixState(_)), %retract it, we need to replace it by the current one
        assertz_fact(mixState((Cs31, Cs3_SAFE1))),
        %write('gone frm here 3'), nl,
        dnf(Cs31, Cs3_SAFE1, InitVars, DnfF),
        %write('dnf: '), write(DnfF), nl,
        %length(DnfF, DLen),
        %write('DNF Size in itr '), write(K), write(': '), write(DLen), nl,
        InitPred=..[init|InitVars],
        constraintInitPreds(Prog1, InitPred, DnfF),
        constraintInitPreds(Prog_SAFE, InitPred, DnfF),
        reset_trConstr_facts,
        initialise_tr_constrs,
        %write('calling abstract_refine'), nl,
        abstract_refine(InitVars, Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,Prog_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE).




abstract_refine(_, bounded(Itr), _, Prog, K, _, Itr, _, F_Int, F_QA, QA_CPA, _, F_SP,F_WidenPoints, _, F_Threshold, _, _, _, F_REFINE, Prog_SAFE,  _,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, _, F_SP_SAFE,F_WidenPoints_SAFE, _, F_THRESHOLD_SAFE, _, _, _, F_REFINE_SAFE):-
	K >= Itr, % Exceeded allowed number of iterations, stop
     verbose_message(['Reqd nr of iterations reached: ']), verbose_message([K]),
    %but still specialises the constraints
    verbose_message(['preprocessing unsafe files ']),
    preProcessHorn(Prog, F_Int, F_QA, QA_CPA, F_WidenPoints, F_Threshold, F_SP),
    verbose_message(['preprocessing safe files ']),
    preProcessHorn(Prog_SAFE, F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_WidenPoints_SAFE, F_THRESHOLD_SAFE, F_SP_SAFE),
    process_call(path('cp'), [F_SP, F_REFINE],[]),
    process_call(path('cp'), [F_SP_SAFE, F_REFINE_SAFE],[]),
	!.


abstract_refine(InitVars, Bounded, LogS,  Prog1, K, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,Prog_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE) :-
	verifyCPA(Prog1, F_Int, F_QA, QA_CPA, F_CPA, F_SP, F_WidenPoints, F_TRACETERM, F_THRESHOLD, Ret1),
    verifyCPA(Prog_SAFE, F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE, F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, Ret1_SAFE),
    %TOFIX: Ret1_SAFE=safe means that there is no clause of the form safe:- (this literally means that there are no safe traces at all), for reuse we have used false instead of safe
( (Ret1 = safe; Ret1_SAFE = safe) ->
        verbose_message(['exit one of the program is safe']),
        %write('=========exit one of the program is safe================'), nl,
        (Ret1 = safe ->   Result = Ret1; true),
        (Ret1_SAFE = safe ->   Result_SAFE = Ret1_SAFE; true),
	    K2 = K,
        process_call(path('cp'), [F_SP, F_REFINE],[]),
        process_call(path('cp'), [F_SP_SAFE, F_REFINE_SAFE],[]),
	    verbose_message(['one of the program is safe'])
    ; (Ret1 = unsafe, Ret1_SAFE = unsafe) ->
        %remove those states from the set of initial states of the safe/unsafe program
        verbose_message(['the cEx from the both safe/unsafe prog. is feasible']),
        feasibleTraceConstraint(unsafeCls, F_TRACETERM, F_SP, InitTraceConstr, InitStateVars),
        refineHorn(F_SP, F_FTA, F_DFTA,  F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant),
        %write('refining unsafe'), nl,
        feasibleTraceConstraint(safeCls, F_TRACETERM_SAFE, F_SP_SAFE, InitTraceConstr_SAFE, InitStateVars_SAFE),
        %write('====== unsafe'), nl,
        refineHorn(F_SP_SAFE, F_FTA_SAFE, F_DFTA_SAFE,  F_SPLIT_SAFE, F_TRACETERM_SAFE, F_REFINE_SAFE, WithInterpolant),
        K1 is K + 1,
        %write('Iteration==================== '), write(K1),nl,
        %write('calling abstract_refine1==============='), nl,
        abstract_refine1(InitVars, Bounded, LogS,  F_REFINE, K1, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP, F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,F_REFINE_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE),
	    verbose_message(['Both the programs are unsafe'])
	; (Ret1 = unsafe) ->
        %remove those states from the set of initial states of the unsafe program
        %refine safe program using fta approach
        verbose_message(['the cEx from unsafe prog. is feasible']),
        feasibleTraceConstraint(unsafeCls,F_TRACETERM, F_SP, InitTraceConstr, InitStateVars),
        refineHorn(F_SP, F_FTA, F_DFTA,  F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant),
        refineHorn(F_SP_SAFE, F_FTA_SAFE, F_DFTA_SAFE,  F_SPLIT_SAFE, F_TRACETERM_SAFE, F_REFINE_SAFE, WithInterpolant),
        K1 is K + 1,
         %write('Iteration==================== '), write(K1),nl,
        %write('calling abstract_refine1==============='), nl,
        abstract_refine1(InitVars, Bounded, LogS,  F_REFINE, K1, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP, F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE,F_REFINE_SAFE,  Result_SAFE, F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE),
	    verbose_message(['The program is unsafe'])
    ;   (Ret1_SAFE = unsafe) ->
        % the trace from the unsafe program is infeasible and from the safe is feasible
        verbose_message(['the cExs from the  safe program is feasible']),
        refineHorn(F_SP, F_FTA, F_DFTA,  F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant),

        feasibleTraceConstraint(safeCls, F_TRACETERM_SAFE, F_SP_SAFE, InitTraceConstr_SAFE, InitStateVars_SAFE),
        refineHorn(F_SP_SAFE, F_FTA_SAFE, F_DFTA_SAFE,  F_SPLIT_SAFE, F_TRACETERM_SAFE, F_REFINE_SAFE, WithInterpolant),
        K1 is K + 1,
        %write('calling abstract_refine1==============='), nl,
        abstract_refine1(InitVars, Bounded, LogS,  F_REFINE, K1, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE, F_REFINE_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE)
    ;
    % traces from both the programs are infeasible, refine them using FTAS
        verbose_message(['the cExs from both the program are infeasible']),
        refineHorn(F_SP, F_FTA, F_DFTA,  F_SPLIT, F_TRACETERM, F_REFINE, WithInterpolant),
        refineHorn(F_SP_SAFE, F_FTA_SAFE, F_DFTA_SAFE,  F_SPLIT_SAFE, F_TRACETERM_SAFE, F_REFINE_SAFE, WithInterpolant),
        %write('refining infeasible clauses'), nl,
        K1 is K + 1,
        %write('calling abstract_refine1==============='), nl,
        abstract_refine1(InitVars, Bounded, LogS,  F_REFINE, K1, Result, K2, WithInterpolant, F_Int, F_QA, QA_CPA, F_CPA, F_SP,F_WidenPoints, F_TRACETERM, F_THRESHOLD, F_FTA, F_DFTA, F_SPLIT, F_REFINE, F_REFINE_SAFE,  Result_SAFE,   F_Int_SAFE, F_QA_SAFE, QA_CPA_SAFE, F_CPA_SAFE, F_SP_SAFE,F_WidenPoints_SAFE, F_TRACETERM_SAFE, F_THRESHOLD_SAFE, F_FTA_SAFE, F_DFTA_SAFE, F_SPLIT_SAFE, F_REFINE_SAFE)
	).

%===============abstract_refine with iteration count end ===================


hornSpecialise(Prog, OutputFile):-
	% TODO: Not using '-raf' option, fix
	prepare_resultdir(Prog, ResultDir),
	path_basename(Prog, F),
	createTmpFilePP(ResultDir, F, F_Int, F_QA, QA_CPA,_,_,F_WidenPoints, _, F_THRESHOLD),
	statistics(runtime,[START|_]),
	preProcessHorn(Prog, F_Int, F_QA, QA_CPA, F_WidenPoints, F_THRESHOLD, OutputFile),
	statistics(runtime,[END|_]),
	DIFF is END - START,
	path_basename(Prog, F),
	verbose_message(['Total time: ', DIFF, ' ms.']),
	end_resultdir(ResultDir).

% ---------------------------------------------------------------------------
% (Temporary directory for intermediate passes)

prepare_resultdir_non-term(Prog, ResultDir) :-
	( opt_debug_temps ->
	    atom_concat(Prog, '_NT_output', ResultDir),
	    mkpath(ResultDir)
	; mktempdir_in_tmp('NonTerm-Horn-XXXXXXXX', ResultDir)
	).

prepare_resultdir(Prog, ResultDir) :-
	( opt_debug_temps ->
	    atom_concat(Prog, '_pihorn_output', ResultDir),
	    mkpath(ResultDir)
	; mktempdir_in_tmp('pihorn-XXXXXXXX', ResultDir)
	).

prepare_safe_resultdir(Prog, ResultDir) :-
	( opt_debug_temps ->
	    atom_concat(Prog, '_pihorn__safe_output', ResultDir),
	    mkpath(ResultDir)
	; mktempdir_in_tmp('pihorn-safe-XXXXXXXX', ResultDir)
	).

end_resultdir(ResultDir) :-
	( opt_debug_temps ->
	    format("NOTE: Files for temporary results are kept at ~w~n", [ResultDir])
	; % remove the directory of intermediate files
	  ( file_exists(ResultDir) -> rmtempdir(ResultDir)
	  ; true
	  )
	).

% ---------------------------------------------------------------------------

% if F_CPA exists it shows the model from F_CPA, else from QA_CPA
% if F_REFINE exists then the model corresponds to this else to Prog

showModel(QA_CPA, F_CPA,Prog, F_REFINE):-
	write('Model: '), nl,
	( file_exists(F_CPA) -> showInv(F_CPA) ; showInvQA(QA_CPA) ),
	nl,
	write('For the program: '), nl,
	( file_exists(F_REFINE) -> showProg(F_REFINE) ; showProg(Prog) ),
	nl.

writeClsListToFile(ClsList, File):-
    open(File, append, S),
    writeClsList(ClsList, S),
    close(S).
writeClsList([(H,B)|Cls], S):-
	numbervars((H, B), 0, _),
    %write((H, B)), nl,
	writeq(S, H),
	write(S, ' :- '),
	list2Conj(B, B1),
	write(S, B1),
	write(S, '.'),
	nl(S),
    writeClsList(Cls, S).
writeClsList([], _).

showInv(F):-
	open(F, read, S),
	read(S, Inv),
	writeToConsole(S, Inv),
	close(S).

writeToConsole(_, end_of_file):-
	!.
writeToConsole(S, Inv):-
	numbervars(Inv, 0, _),
	write('.'),
	nl,
	read(S, Inv1),
	writeToConsole(S, Inv1).

showProg(F):-
	load_file(F),
	writeCls.

writeCls:-
	my_clause(H, B, _),
	numbervars((H, B), 0, _),
	writeq(H),
	write(' :- '),
	list2Conj(B, B1),
	write(B1),
	write('.'),
	nl,
	fail.
writeCls.


showInvQA(F):-
	open(F, read, S),
	read(S, Inv),
	writeToConsoleQA(S, Inv),
	close(S).

writeToConsoleQA(_, end_of_file):-
	!.
writeToConsoleQA(S, (H:-Inv)):-
	stripSuffix(H, H1),
	numbervars((H1:-Inv), 0, _),
	write((H1:-Inv)),
	write('.'),
	nl,
	read(S, Inv1),
	writeToConsoleQA(S, Inv1).

stripSuffix(F,F1) :-
	F =.. [P|Xs],
	name(P,PName),
	removeSuffixChars(PName,P1Name),
	name(P1,P1Name),
	F1 =.. [P1|Xs],
	!.
	
removeSuffixChars(FName,F1Name) :-
	append("_query",_,Suff),
	append(F1Name,Suff,FName),
	!.
removeSuffixChars(FName,F1Name) :-
	append("_ans",_,Suff),
	append(F1Name,Suff,FName),
	!.
removeSuffixChars(FName,FName).


raf_file(ResultDir, F, F_Raf) :-
	atom_concat(F, '.raf.pl', F_Raf0),
	path_concat(ResultDir, F_Raf0, F_Raf).

wideningPoints_file(ResultDir, F_WidenPoints) :-
	path_concat(ResultDir, 'widenpoints', F_WidenPoints).

traceTerm_file(ResultDir, F_Traceterm) :-
	path_concat(ResultDir, 'traceterm.out', F_Traceterm).

threshold_file(ResultDir, F_Threshold) :-
	path_concat(ResultDir, 'wut.props', F_Threshold).

int_file(ResultDir, F, F_Int) :-
	atom_concat(F, '.int.pl', F_Int0),
	path_concat(ResultDir, F_Int0, F_Int).

qa_file(ResultDir, F, F_QA) :-
	atom_concat(F, '.qa.pl', F_QA0),
	path_concat(ResultDir, F_QA0, F_QA).

qa_cpa_file(ResultDir, F, QA_CPA) :-
	atom_concat(F, '.qa.pl.cha.pl', QA_CPA0),
	path_concat(ResultDir, QA_CPA0, QA_CPA).
cpa_file(ResultDir, F, CPA) :-
	atom_concat(F, '.cha.pl', CPA0),
	path_concat(ResultDir, CPA0, CPA).

sp_file(ResultDir, F, F_SP) :-
	atom_concat(F, '.pe.pl', F_SP0),
	path_concat(ResultDir, F_SP0, F_SP).

fta_file(ResultDir, F, F_FTA) :-
	atom_concat(F, '.fta.pl', F_FTA0),
	path_concat(ResultDir, F_FTA0, F_FTA).

dfta_file(ResultDir, F, F_DFTA) :-
	atom_concat(F, '.dfta.pl', F_DFTA0),
	path_concat(ResultDir, F_DFTA0, F_DFTA).

split_file(ResultDir, F, F_SPLIT) :-
	atom_concat(F, '.split.pl', F_SPLIT0),
	path_concat(ResultDir, F_SPLIT0, F_SPLIT).

refine_file(ResultDir, F, F_REFINE) :-
	atom_concat(F, '.refine.pl', F_REFINE0),
	path_concat(ResultDir, F_REFINE0, F_REFINE).

safe_file(ResultDir, F, F_SAFE) :-
	atom_concat(F, '.safe.pl', F_SAFE0),
	path_concat(ResultDir, F_SAFE0, F_SAFE).

createTmpFilePP(ResultDir, F, F_Int, F_QA, QA_CPA, F_CPA, F_SP, F_WidenPoints, F_Traceterm, F_Threshold):-
	int_file(ResultDir, F, F_Int),
	qa_file(ResultDir, F, F_QA),
	qa_cpa_file(ResultDir,F, QA_CPA),
	sp_file(ResultDir, F, F_SP),
	wideningPoints_file(ResultDir, F_WidenPoints),
	traceTerm_file(ResultDir, F_Traceterm),
	threshold_file(ResultDir, F_Threshold),
	cpa_file(ResultDir, F, F_CPA).

createTmpFileRef(ResultDir, F, F_FTA, F_DFTA, F_SPLIT, F_REFINE):-
	fta_file(ResultDir, F, F_FTA),
	dfta_file(ResultDir, F, F_DFTA),
	split_file(ResultDir, F, F_SPLIT),
	refine_file(ResultDir, F, F_REFINE).

createTmpFileNonTerm(ResultDir, F, F_Verify_Loop_Exit, F_Verify_Loop_Entry, F_Split):-
	loop_exit_file(ResultDir, F, F_Verify_Loop_Exit),
    loop_entry_file(ResultDir, F, F_Verify_Loop_Entry),
    cls_split_file(ResultDir, F, F_Split).

cls_split_file(ResultDir, F, F_Split) :-
	atom_concat(F, '.clsplit.pl', F_Split1),
	path_concat(ResultDir, F_Split1, F_Split).

loop_exit_file(ResultDir, F, F_Verify_Loop_Exit) :-
	atom_concat(F, '.loop_exit.pl', F_Loop_Exit),
	path_concat(ResultDir, F_Loop_Exit, F_Verify_Loop_Exit).

loop_entry_file(ResultDir, F, F_Verify_Loop_Entry) :-
	atom_concat(F, '.loop_entry.pl', F_Loop_Entry),
	path_concat(ResultDir, F_Loop_Entry, F_Verify_Loop_Entry).

% Cs is a list of list representing a disjunction
getInitCondFile(File, Cs):-
    precond(File,Ls),
    %write('init cond after  transformation '), write(Ls),nl,
    getInitCond(Ls, Cs).

getInitCond([(_,[])|_], [[true]]):-
    !. %if an init cond is true, then the disj is true as well
getInitCond([(_,C)], [C]).
getInitCond([], [[false]]).
getInitCond([(_,C)|Ls], [C|Cs]):-
    getInitCond(Ls, Cs).


%removes from Cs, the existential vars specified in ExistsVars and retruns a makePolyhedron with variables removed
removeExistsVars(Cs, ExistsVars, Cs1):-
    makePolyhedron(Cs, Ps),
    project(Ps, ExistsVars, Ps),
    getConstraint(Ps, Cs1).


% FTrace is known to be feasible
%FileType: file containing either false:- or safe:- clauses, which can be either unsafeCls or safeCls
feasibleTraceConstraint(FileType, FTrace, Prog, InitTraceConstr, InitStateVars):-
    readCex(FTrace, Cex),
    getCexConstr(Prog, Cex, InitTraceConstr, InitStateVars),
    yices_init,
    (FileType=unsafeCls ->
        (InitTraceConstr=[] ->
            retractall_fact(trConstr(_)),
            retractall_fact(trConstrAcc(_)),
            assertz_fact(trConstr([true])),
            assertz_fact(trConstrAcc(true))
        ;
            cond_assert_cex_tr(unsafeCls, InitTraceConstr, InitStateVars)
        )
    ;
       (InitTraceConstr=[] ->
            retractall_fact(trConstrSafe(_)),
            retractall_fact(trConstrSafeAcc(_)),
            assertz_fact(trConstrSafe([true])),
            assertz_fact(trConstrSafeAcc(true))
        ;
            cond_assert_cex_tr(safeCls, InitTraceConstr, InitStateVars)
        )
    ),
    yices_exit,
    write('cex constr ====== '), write(FileType), write('=== '), write(InitTraceConstr), nl.

cond_assert_precond(Cs3_SAFE1,Cs31):-
    %((Cs3_SAFE1=[], Cs31=[]);(Cs3_SAFE1=[[]], Cs31=[[]]);(Cs3_SAFE1=[false], Cs31=[false]       ;%(Cs3_SAFE1=[[false]], Cs31=[[false]]))->true
    %;
%write('safe  =1= '), write(Cs3_SAFE1), nl,
    %    write('unsafe  =1= '), write(Cs31), nl,
    %syntactic check for equality
    (Cs3_SAFE1==Cs31 ->   true
    ;
        %write('================wrongness could be here================== '), nl,
        %write('safe  == '), write(Cs3_SAFE1), nl,
        %write('unsafe  == '), write(Cs31), nl,
        assertz_fact(precond((Cs3_SAFE1, Cs31)))
    ).

cond_assert_cex_tr(FileType, InitTraceConstr, Vars):-
    (FileType=unsafeCls ->
        (trConstrAcc(AC)->
            (implies(InitTraceConstr, AC, Vars)->
                true %the stored constrained is already more general
            ;
            (implies(AC, InitTraceConstr, Vars)->
                retractall_fact(trConstr(_)),
                retractall_fact(trConstrAcc(_)),
                assertz_fact(trConstr(InitTraceConstr)),
                assertz_fact(trConstrAcc(InitTraceConstr))
            ;
            %keep  both
                retractall_fact(trConstrAcc(_)),
                assertz_fact(trConstr(InitTraceConstr)),
                assertz_fact(trConstrAcc((InitTraceConstr;AC)))

            )
            )
        ;
            assertz_fact(trConstr(InitTraceConstr)),
            assertz_fact(trConstrAcc(InitTraceConstr))
        )
    ;
        (trConstrSafeAcc(AC)->
            (implies(InitTraceConstr, AC, Vars)->
                true %the stored constrained is already more general
            ;
            (implies(AC, InitTraceConstr, Vars)->
                retractall_fact(trConstrSafe(_)),
                retractall_fact(trConstrSafeAcc(_)),
                assertz_fact(trConstrSafe(InitTraceConstr)),
                assertz_fact(trConstrSafeAcc(InitTraceConstr))
            ;
            %keep  both
                retractall_fact(trConstrSafeAcc(_)),
                assertz_fact(trConstrSafe(InitTraceConstr)),
                assertz_fact(trConstrSafeAcc((InitTraceConstr;AC)))

            )
            )
        ;
            assertz_fact(trConstrSafe(InitTraceConstr)),
            assertz_fact(trConstrSafeAcc(InitTraceConstr))
        )
    ).



getCexConstr(F, Cex, InitTraceConstr, InitStateVars):-
    load_file(F),
    start_ppl,
    checkTrace([false],[Cex], [],TraceContr, [], InitStateVars),
    %write('after'), write(InitStateVars), nl,
    separateLinearConstraints(TraceContr, TraceContrL, _),
    verbose_message(['trace constr ', TraceContr]), nl,
    varset(TraceContrL, TraceVars),
    setdiff(TraceVars, InitStateVars, ExistsVars),
    %write(TraceContrL), nl,
    numbervars((InitStateVars,TraceVars), 0, _),
    %write('trace constr before projection'), write(TraceContrL), nl,
    %write('vars to proj out'), write(ExistsVars), nl,
    %write('init vars of interest'), write(InitStateVars), nl,
    removeExistsVars(TraceContrL, ExistsVars, InitTraceConstr),
    %write('trace constr '), write(TraceContr), nl,
    %write('init contr cex: '), write(InitTraceConstr),nl,
    end_ppl.

%ignores non-linear constraints

retrieveCexConstr(F, Cex, TraceConstr):-
    load_file(F),
    start_ppl,
    checkTrace([false],[Cex], [],TraceContr, [], InitStateVars),
    %write('after'), write(InitStateVars), nl,
    separateLinearConstraints(TraceContr, TraceContrL, _),
    varset(TraceContrL, TraceVars),
    setdiff(TraceVars, InitStateVars, ExistsVars),
    %write(TraceContrL), nl,
    numbervars((InitStateVars,TraceVars), 0, _),
    %write('trace constr before projection'), write(TraceContrL), nl,
    %write('vars to proj out'), write(ExistsVars), nl,
    %write('init vars of interest'), write(InitStateVars), nl,
    removeExistsVars(TraceContrL, ExistsVars,TraceConstr),
    %write('trace constr '), write(TraceContr), nl,
    %write('init contr cex: '), write(InitTraceConstr),nl,
    end_ppl.




%assume init([X])/initXXX([X]) is the initial, and init maynot be in the leaves
/*TODO: the projection still leaves extra variables if a derivation tree uses more than one init predicates,
now the solution is if it finds one, do not search for another*/
checkTrace([], _, Cs, Cs, InitStateVars,InitStateVars).
checkTrace([B|Bs], [T|Ts], Cs,TraceContr, InitVars, AllInitVars) :-
	T =..[C|Ts1],
	my_clause(B,Bs1,C),
    separate_constraints(Bs1,Cs1,Bs2),
    %just select one init atom from the whole trace
    (InitVars=[] -> %just select one init atom from the whole trace
        getInitVarsCls(B, Cs1, InitVarCls),
        InitVars1= InitVarCls
    ;
        InitVars1=InitVars
    ),
	append(Bs2,Bs,Bs3),
	append(Cs1,Cs,Cs2),
	%checkSat(Cs2),
	append(Ts1,Ts,Ts2),
	checkTrace(Bs3,Ts2, Cs2,TraceContr, InitVars1, AllInitVars).

%init is in the head
getInitVarsCls(B, _, InitVarCls):-
    functor(B,F,_),
    atom(F),
    atom_concat(init, _, F),
    !,
    B=..[_|InitVarCls].
%init()=init() maybe in the body
getInitVarsCls(_, Body, InitVarCls):-
    !,
    getInitVarsFromBody(Body, InitVarCls).

getInitVarsFromBody([], []).
%assume there is only one init/initXXX in the body as well
getInitVarsFromBody([_=P|_], InitVarsCs1):-
    functor(P,F,_), atom(F), atom_concat(init, _, F),
    !,
    P=..[_|InitVarsCs1].
getInitVarsFromBody([_|Bs], InitVarsCs1):-
    getInitVarsFromBody(Bs, InitVarsCs1).

%combines the precondition, Cs, with those derived from the counterexample
combineTraceConstraints(FileType, Cs, FCs, InitVars):-
    (FileType=unsafeCls ->
        findall(C, (trConstr(C), verbose_message([' trace unsafe: ', C])), TCs)
    ;
        findall(C, (trConstrSafe(C), verbose_message([' trace safe: ', C])), TCs)
    ),
    yices_init,
    verbose_message([FileType, ' TCS ', TCs]),
    verbose_message(['cond prog ', Cs]),
    %write('=================== traces ============================ '), write(TCs), nl,
    %write(InitVars), nl,
    simplify_dnf_2_dnf(Cs,InitVars, Cs1),
    simplify_dnf_2_dnf(TCs,InitVars, TCs1),
    verbose_message(['cond prog1' , Cs1]),
    verbose_message(['traces1 ', TCs1]),
    %write(TCs1), nl,
    (TCs1=[] ->
        FCs=Cs1;
    (Cs1=[]; Cs1=true ->
        FCs=TCs1
    ;
        append(Cs1, TCs1, Approxs),
        verbose_message(['approx ', Approxs]),
        simplify_dnf_2_dnf(Approxs, InitVars, FCs)
    )
    ),
    verbose_message(['combined constr ', FileType, FCs]),
    yices_exit.


check_unsat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_unsat(Formula,VInts).

check_sat(Formula, Vs):-
	yices_vars(Vs, int, VInts),
	yices_sat(Formula,VInts).

check_unsat_w_res(Formula, Vs, Res):-
    (check_unsat(Formula, Vs)-> Res=unsat; Res=sat).

check_tautology_w_res(Formula, Vs, Res):-
    (tautology(Formula, Vs)-> Res=tautology; Res=not-tautology).


/*
constrains init states to the conj of safe and unsafe over-approximation given in dnf form by DnfF

*/
constraintInitPreds(F, InitPred, DnfF):-
    write('begin file '), write(F), nl,
    %write('init pred '), write(InitPred), nl,
    PF='prop.pl',
    Refined_File='tmp.pl',
    create_file(PF),
    open(PF, write, Sp),
    write('dnf ================ '), write(DnfF), nl,
    write_disj(DnfF, InitPred, Sp),
    close(Sp),
    (preserveInitPred->
        constrainInitStates:main(['-prg',F,'-pre',PF, '-o', Refined_File])
    ;
        %all-cls option ensures all cls with init pred will be constrained
        constrainInitStates:main(['-prg',F,'-all-cls','-pre',PF, '-o', Refined_File])
    ),
    %write('inserting '),  nl,
    process_call(path('mv'), [Refined_File, F],[]),
    delete_file(PF).


write_disj((C;R), InitPred, Sp):-
    write_disj(C, InitPred, Sp),
    !,
    write_disj(R, InitPred, Sp).
write_disj(C, InitPred, Sp):-
    write(Sp, InitPred),
    write(Sp, ':- '),
    %write(C), nl,
    write(Sp, C),
    write(Sp, '.'),
    nl(Sp).




create_file(F):-
    process_call(path('touch'), [F],[]).
delete_file(F):-
    process_call(path('rm'), [F],[]).

%remove false:- clauses and replace safe:- clauses by false:- clauses
replaceSafePred(F, O):-
    load_file(F),
    retractall_fact(my_clause(false, _, _)),
    replaceSafeByFalse,
    dumpClstoFile(O).

replaceSafeByFalse:-
    my_clause(safe, B,C),
    retract(my_clause(safe, _, C)),
    assertz_fact(my_clause(false, B, C)),
    fail.
replaceSafeByFalse.

writeClsToFile(S):-
	my_clause(H, B, _),
	numbervars((H, B), 0, _),
	writeq(S,H),
	write(S,' :- '),
	list2Conj(B, B1),
	write(S,B1),
	write(S,'.'),
	nl(S),
	fail.
writeClsToFile(_).


dumpClstoFile(F):-
	open(F, write, S),
	writeClsToFile(S),
	close(S).

%check if approximation of safe (S) /\ unsafe (U) is unsat, then the precond are optimal
unsatApproxSafeUnsafe(S, U,  Vs):-
    listofList2YicesDisj(S, DS),
    listofList2YicesDisj(U, DU),
    yices_init,
    (check_unsat((DS,DU),Vs)-> yices_exit; yices_exit, !, fail). %just to close yices handle
