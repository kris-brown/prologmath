:- module(fact,
  [facts/1, facts/2, allfacts/1, allfacts/2, % intended functions to be exposed user
   fact/5   % to be used by other modules to add to common database
    ]).

:- multifile fact/5.

/*
Launch queries to identify all facts whose pattern matches a given input

Facts: Give a name, page number, description, and pattern
- whenever the pattern is matched, then the exported funcs
  (facts,allfacts) will print the fact to the screen
- add constraints to the pattern by adding clauses
- the order of items in the pattern list will not matter in the end

The fact constructor can be used in other modules to add to knowledge base
*/
fact("testfact",1,[1],1,"Just a test").

%%% User-exposed functions:

% Note: "facts" prints only facts which incorporate everything in the input list
facts(InputPattern) :-
 facts(InputPattern, []). % default argument for result is to match on anything

facts(InputPattern,ResultPattern) :-
 sortlen(InputPattern,N,IP_no_dups), % remove duplicates (they're irrelevant)
 allFactMatchN(IP_no_dups,ResultPattern,N,Res),
 print_facts(Res).

% Returns any fact that can be matched to a subset of input list
allfacts(InputPattern) :-
 allfacts(InputPattern, []).% default argument for result is to match on anything

allfacts(InputPattern,ResultPattern) :-
 sortlen(InputPattern,N,IP_no_dups), % remove duplicates (they're irrelevant)
 fmloop(IP_no_dups,ResultPattern,N,Res),
 print_facts(Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Infrastructure for matching arbitrary inputs to facts %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Helper functions for fact-finding infrastructure
% Sort, remove duplicates, and get length
sortlen(X,N,X_no_dups) :-
  length(X,N),
  sort(X,X_no_dups).

% List with some element removed
del(X,[X|L],L).
del(X,[Y|L],[Y|L1]) :- del(X,L,L1).

% Any N-length permutation of elements of some list
perm(_, 0, []).
perm(Xs, N, Ys) :-
    N1 is N-1,
    member(X,Xs),
    del(X,Xs,NewXs),
    perm(NewXs, N1, NewYs),
    Ys = [X|NewYs].

permutations(X, N, Y) :- findall(Z, perm(X, N, Z), Y).

% Rendering tuple of fact data
print_facts([]).
print_facts([H|T]) :-
  format('~n**************************************~n~w (Page ~w) ~n~t~w~n~n',H),
  print_facts(T).

%%% Heavyweight functions

% Does some input match the pattern of some fact of arity N (return fact summary)
% Would be nice if we could tell which inputs were bound to which elements in the
% fact pattern.
factmatchN(InPat,ResPat,Len,Results) :-
  perm(InPat,Len,Perm),                 % Perm is some N-length permutation of input
  ((ResPat = [],
   fact(FactName,Page,Perm,_,Desc));
   fact(FactName,Page,Perm,ResPat,Desc)), % any possible fact w/ unifiable input AND output pattern
  Results = [FactName,Page,Desc]. % if successful, return details about the fact

% Remove duplicate results  via setof (e.g. input is [2,4,6], we'll have lots
% of permutations that match a pattern of a single even number)
allFactMatchN(X,Y,N,Results) :-
  findall(Res,factmatchN(X,Y,N,Res),DupResults),
  sort(DupResults,Results).

% FactMatch Loop
fmloop(_, _, 0, []) :- !. % we go to infinite loop if we don't CUT here!
fmloop(X,Y,N,Results) :-
  N1 is N - 1,  % loop variable
  length(X, M), % Sanity check - we shouldn't be calling this with N > M
  N1 <  M,      % (i.e. looking for facts w/ greater arity than our input itself)
  allFactMatchN(X,Y,N,Res1),   % list of facts of arity N that match input X
  fmloop(X,Y,N1,Res2),         % list of facts with arity lower than N
  append(Res1,Res2,Results). % concatenate lists
