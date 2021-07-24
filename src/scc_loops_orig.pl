:- module(scc_loops, _).

% Strongly connected components based
% on depth-first search of a graph.
% Algorithm by M. Sharir, adapted from
% Baase and Van Gelder, Chapter 7.5
% JPG 20/8/01

% added computation of widening points, i.e. for each recursive component, a set of nodes
% that cuts every loop


:- use_module(chclibs(load_simple)).
:- use_module(chclibs(balanced_tree)).
:- use_module(library(lists)).
:- use_module(chclibs(builtins)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(common)).



% scc(Ps,Prog,Cs):   
% scc(+,+,-):
%	Ps:  a list of predicates.
%     Prog:  list of clauses (read in by readprog/2)
%	Cs:  a list of components, each labelled as recursive or non-recursive
%        the recursive components include a list of widening points
%

% fscc(File,Cs):   
% fscc(+,-):
%	F:  a file containing a logic program
%	Cs:  a list of components, each labelled as recursive or non-recursive
%

main([F]) :-
	fscc(F,SCCs,BEs),
	write(SCCs),
	nl,nl,
	write(BEs),
	nl.

fscc(F,SCCs,BEs) :-
	load_file(F),
	dependency_graph(Es,Vs),
	scc_graph(Es,Vs,SCCs,BEs).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate dependency graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dependency_graph(Es,Vs) :-
	(setof(P/N-Q/M, [H,Bs,B,C,BC]^(
			my_clause(H,Bs,C),
			functor(H,P,N),
			member(B,Bs),
			\+ constraint(B,BC),
			functor(B,Q,M)
			),
			Es) -> true;
		Es = []),
	(setof(A, [X,Y,H,B,C,P,N]^(
			(member(X-Y,Es),
			(A=X; A=Y);
			my_clause(H,B,C),
			functor(H,P,N),
			A = P/N)
			),
			Vs) -> true; Vs = []).


% compute sccs of a digraph (given as a list of edges and vertices)
% scc_graph([a-b,a-c,b-d,....], [a,b,c,...], Y).

scc_graph(Edges,Vs, SCCs, BEs) :-
	makeGraph(Edges,Vs,G),
	scc_sharir(G,SCCs),
	entryPreds(G,Es),
	append(Es,Vs,Vs1),
	%(entryPred(Vs,G,E) -> Vs1 = [E|Vs]; 
	% sinkPred(Vs,G,E)  -> Vs1 = [E|Vs]; 
	% Vs1 = Vs),
	backEdgesSweep(Vs1,G,[],root,_,BEs,[]).
	
	%entryPred(Vs,G,Entry),
	%backEdges(G,Entry,[],root,_,BEs,[]).
	%reducedGraph(SCCs,G,R).

% scc_sharir: the SCC procedure.

scc_sharir(root,[]) :-
	!.
scc_sharir(Graph,SCCs) :-
	phase1(Graph,Stack),
	phase2(Stack,Graph, SCCs),
	!,
	recursive_classify(SCCs,Graph).
	
phase1(Graph,Stack) :-
	traversekey_tree(Graph,Nodes),
	dfsSweep(Nodes,Graph,root,_,[],Stack).

dfsSweep([], _, MarkList, MarkList, Stack, Stack).
dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, StackIn, StackOut) :-
	search_tree(MarkListIn,N,black),   % N already visited
	!,
	dfsSweep(Ns, Graph, MarkListIn, MarkListOut, StackIn, StackOut).
dfsSweep([N|Ns], Graph, MarkListIn, MarkListOut, StackIn, StackOut) :-
	dfsNode(Graph, N, MarkListIn, MarkListMid, StackIn, StackMid),
	dfsSweep(Ns, Graph, MarkListMid, MarkListOut, StackMid, StackOut).

dfsNode(Graph,N,M0,M2,S0,S1) :-
	insert_tree(M0,N,black,M1),   % mark node as visited
	find_succs(Graph,N,SuccList),
	dfs_each(SuccList,Graph,N,M1,M2,S0,S1).

dfs_each([],_,Par,M,M,S,[Par|S]).
dfs_each([N|Ns],G,Par,M0,M1,S0,S1) :-
	search_tree(M0,N,black),
	!,
	dfs_each(Ns,G,Par,M0,M1,S0,S1).
dfs_each([N|Ns],G,Par,M0,M2,S0,S2) :-
	dfsNode(G,N,M0,M1,S0,S1),
	dfs_each(Ns,G,Par,M1,M2,S1,S2).

find_succs(Graph,N,SuccList) :-
	search_tree(Graph,N,links(SuccList,_)),
	!.
find_succs(_,_,[]).

find_preds(Graph,N,PredList) :-
	search_tree(Graph,N,links(_,PredList)),
	!.
find_preds(_,_,[]).


% phase 2:  use the depth-first ordering from phase 1
% to traverse the transposed graph.

phase2(Nodes,Graph,SCCs) :-
	dfsSweep2(Nodes,Graph,root,_,[],SCCs).

dfsSweep2([], _, MarkList, MarkList, S,S).
dfsSweep2([N|Ns], Graph, MarkListIn, MarkListOut, S0,S1) :-
	search_tree(MarkListIn,N,black),  % N already visited
	!,
	dfsSweep2(Ns, Graph, MarkListIn, MarkListOut, S0,S1).
dfsSweep2([N|Ns], Graph, MarkListIn, MarkListOut, S0,S2) :-
	dfsNode2(Graph, N,  MarkListIn, MarkListMid,SCC,[]),
	dfsSweep2(Ns, Graph, MarkListMid, MarkListOut, [(_,SCC)|S0],S2).

dfsNode2(Graph,N,M0,M2,[N|S0],S1) :-
	insert_tree(M0,N,black,M1),  % mark node as visited
	search_tree(Graph,N,links(_,PrecList)),
	dfs_each2(PrecList,Graph,M1,M2,S0,S1).

dfs_each2([],_,M,M,S,S).
dfs_each2([N|Ns],G,M0,M1,S0,S1) :-
	search_tree(M0,N,black),
	!,
	dfs_each2(Ns,G,M0,M1,S0,S1).
dfs_each2([N|Ns],G,M0,M2,S0,S2) :-
	dfsNode2(G,N,M0,M1,S0,S1),
	dfs_each2(Ns,G,M1,M2,S1,S2).

recursive_classify([],_).
recursive_classify([(recursive,[_,_|_])|Cs],G) :-
	!,
	recursive_classify(Cs,G).
recursive_classify([(recursive,[P])|Cs],G) :-
	direct_recursive(P,G),
	!,
	recursive_classify(Cs,G).
recursive_classify([(non_recursive,_)|Cs],G) :-
	recursive_classify(Cs,G).

direct_recursive(P,G) :-
	search_tree(G,P,links(Ss,_)),
	member(P,Ss).
	
% starting from a list of vertices and links,
% make an adjacency list representation of the graph 
% and the transposed graph (reversing links).

makeGraph(Es,Vs,G) :-
	insertVertices(Vs,root,G0),
	makeGraph1(Es,G0,G).
	
makeGraph1([],G,G).
makeGraph1([A-B|Ls],G0,G3) :-
	insert_succ_link(A,B,G0,G1),
	insert_pred_link(B,A,G1,G2),
	makeGraph1(Ls,G2,G3).
	
insertVertices([],G,G).
insertVertices([V|Vs],G0,G2) :-
	insert_tree(G0,V,links([],[]),G1),
	insertVertices(Vs,G1,G2).

insert_succ_link(Q,P,G0,G1) :-
	search_replace_tree(G0,Q,links(Ps,Ss),G1,links(Ps1,Ss)),
	!,
	setunion([P],Ps,Ps1).
insert_succ_link(Q,P,G0,G1) :-
	insert_tree(G0,Q,links([P],[]),G1).

insert_pred_link(Q,P,G0,G1) :-
	search_replace_tree(G0,Q,links(Ps,Ss),G1,links(Ps,Ss1)),
	!,
	setunion([P],Ss,Ss1).
insert_pred_link(Q,P,G0,G1) :-
	insert_tree(G0,Q,links([],[P]),G1).

naturalLoops(Es,Vs,Loops) :-
	makeGraph(Es,Vs,G),
	entryPred(Vs,G,Entry),
	dominators(Entry,Vs,G,Ds),
	backEdges(G,Entry,[],root,_,BEs,[]),
	natLoops(BEs,Ds,G,Loops).

entryPred(Vs,G,V) :-
	member(V,Vs),
	search_tree(G,V,links(_,[])), 	% find the entry predicate - no predecessors
	!.
	
entryPreds(G,Es) :-
	traverse_tree(G,Rs),
	entryNodes(Rs,Es).
	
entryNodes([rec(N,links(_,[]))|Rs],[N|Ns]) :-
	!,
	entryNodes(Rs,Ns).
entryNodes([_|Rs],Ns) :-
	entryNodes(Rs,Ns).
entryNodes([],[]).
	
	
	
sinkPred(Vs,G,V) :-
	member(V,Vs),
	search_tree(G,V,links([],_)), 	% find the sink predicate - no successors
	!.
	
backEdges(Graph,N,As,M0,M2,B0,B1) :-
	insert_tree(M0,N,black,M1),   % mark node as visited
	find_succs(Graph,N,SuccList),
	backEdgesSweep(SuccList,Graph,[N|As],M1,M2,B0,B1).

backEdgesSweep([], _, _,MarkList, MarkList, B,B).
backEdgesSweep([N|Ns], Graph, As,MarkListIn, MarkListOut, B0,B2) :-
	search_tree(MarkListIn,N,black),   % N already visited
	!,
	(member(N,As), As=[M|_] -> B0=[(M-N)|B1]; B1=B0),
	backEdgesSweep(Ns, Graph, As,MarkListIn, MarkListOut, B1,B2).
backEdgesSweep([N|Ns], Graph, As, MarkListIn, MarkListOut, B0,B2) :-
	backEdges(Graph, N, As, MarkListIn, MarkListMid, B0,B1),
	backEdgesSweep(Ns, Graph, As, MarkListMid, MarkListOut, B1,B2).
	
natLoops([(P-Q)|WPs],Doms,Graph,[(Q,Loop)|Loops]) :-
	dominates(Doms,P,Q),
	!,
	collectLoop([P],Graph,[Q],Loop), % DFS from the back-edge 
	natLoops(WPs,Doms,Graph,Loops).
natLoops([_|WPs],Doms,Graph,Loops) :-
	natLoops(WPs,Doms,Graph,Loops).
natLoops([],_,_,[]).

dominates(Doms,P,Q) :-
	member(dom(P,Vs),Doms),
	member(Q,Vs),
	!.

collectLoop([P|Ps],Graph,Visited0,Visited1) :-
	member(P,Visited0),
	!,
	collectLoop(Ps,Graph,Visited0,Visited1).
collectLoop([P|Ps],Graph,Visited0,Visited2) :-
	search_tree(Graph,P,links(Qs,_)),
	append(Qs,Ps,Ps1),
	collectLoop(Ps1,Graph,[P|Visited0],Visited2).
collectLoop([],_,Vs,Vs).

% Fairly naive implementation of dominators
dominators(Entry,Vs,G,Ds) :-
	initDominators(Vs,Vs,Entry,Ds0),
	iterateDominators(Ds0,Entry,G,Ds).
	
initDominators([],_,_,[]).
initDominators([E|Vs],Vs0,E,[dom(E,[E])|Ds]) :-
	!,
	initDominators(Vs,Vs0,E,Ds).
initDominators([V|Vs],Vs0,E,[dom(V,Vs0)|Ds]) :-
	initDominators(Vs,Vs0,E,Ds).
	
iterateDominators(Ds0,E,G,Ds) :-
	computeDominatorEqn(Ds0,E,G,Ds1,Ds0,Ch),
	(Ch==0 -> Ds = Ds1;
	iterateDominators(Ds1,E,G,Ds)).

computeDominatorEqn([],_,_,[],_,0).
computeDominatorEqn([dom(V,DVs)|Ds0],E,G,[dom(V,DVs)|Ds1],Doms,Ch) :-
	V==E,
	!,
	computeDominatorEqn(Ds0,E,G,Ds1,Doms,Ch).
computeDominatorEqn([dom(V,DVs)|Ds0],E,G,[dom(V,DVs2)|Ds1],Doms,Ch) :-
	search_tree(G,V,links(Ps,_)),
	getPredDoms(Ps,Doms,PsDoms),
	intersectAll(PsDoms,DVs1),	
	setunion([V],DVs1,DVs2),
	checkChange(DVs,DVs2,Ch0),
	computeDominatorEqn(Ds0,E,G,Ds1,Doms,Ch1),
	or(Ch0,Ch1,Ch).
	
getPredDoms([V|Vs],Doms,[DV|DVs]) :-
	member(dom(V,DV),Doms),
	!,
	getPredDoms(Vs,Doms,DVs).
getPredDoms([],_,[]).

intersectAll([],[]).
intersectAll([A],A).
intersectAll([A1,A2|As],S) :-
    intersectAll([A2|As],S1),
    setintersect(A1,S1,S).
    
checkChange(Ds0,Ds1,0) :-  	% Note, Ds1 subset of Ds0, hence no change if Ds0 subset Ds1 
	subset(Ds0,Ds1),
	!.
checkChange(_,_,1).

or(A,B,C) :-
	A==0 -> B=C; C=1.