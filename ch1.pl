:- module(ch1,
  [associative/1,binary/1,group/1,
   bin_type/2, g_set/2,
   app/3,binapp/4]).

:- use_module(fact). % note: atoms do not need to be prefixed, only predicates
:- use_module(simple).
:- multifile fact:fact/5. % extending the definition of a fact

/*
Introduction to Groups
*/

%%%%%%%%%%%%%%
% Predicates %
%%%%%%%%%%%%%%%
binary(X) :- simple:type(X,[Y,Y,Y]).

% Given a binary func, get the set that is its domain/codomain
bin_type(F,S) :-
  binary(F),
  simple:type(F,S,S,S).

associative(z_add).
% A group is uniquely determined by a function (which has a well-defined domain
% and codomain). Not all functions imply a group, so "group" is a predicate for
% functions that can be true or false.
group(F):-
    binary(F),            % F is a binary operation on some set
    associative(F),       % F is associative
    group_identity(F,_),  % there exists a group identity (forall x: e*x = x = x*e)
    has_group_inverses(F).% forall x: exists x_inv: x_inv * x = 1

% Get the set associated with function F that defines a group
g_set(F,S) :-
  group(F),
  bin_type(F,S).
%%%%%%%%%%%%%%%%%%%
% Other Relations %
%%%%%%%%%%%%%%%%%%%

% Function application (easier to describe in this form with noncurried functions)
app(neg_z,X,Y) :- Y is (-1 * X).

% Relation between curried and non-curried application of binary function
binapp(Func,Arg1,Arg2,Res) :-
  app(Func,Arg1,Curried), % won't this fail if no atom for curried exists?
  app(Curried,Arg2,Res).

%%%%%%%%%
% Facts %
%%%%%%%%%

fact:fact("Binary function",16,[Func,Set],"binary",
          "Consider Func as a binary function over Set") :-
          simple:func(Func),
          simple:set(Set),
          simple:type(Func,[Set,Set,Set]).


fact:fact("Prop1.1",18,[G],'',
          "The identity of G is unique") :- group(G).
fact:fact("Prop1.2",18,[G],'',
          "For each a in G, a^-1 is uniquely determined") :- group(G).
fact:fact("Prop1.3",18,[G],'',
          "(a^-1)^-1=a for all a in G") :- group(G).

fact:fact("Prop2.1",20,[G,A,U,V],'',
          "If au=av, then u=v") :-
    group(G),
    g_set(G,S),
    elem(A,S),elem(U,S),elem(V,S).
