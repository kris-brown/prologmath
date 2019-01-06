:- module(chap1,
  [group/2,binary/2,func/1,set/1,z/0,z_add/0,binapp/4,associative/1]).
:- use_module(library(clpfd)).

% General
%fact/4 : name, page, input, description
% Form of query, given some information to start a problem:
%   - fact(Name,Page,Desc,X)

relation(1,func).
relation(1,associative).
relation(2,binary).

% Definition 1
% A binary function over set S has type S -> S -> S
binary(Func,Set) :-
  func(Func),
  set(Set),
  types(Func,[Set,Set,Set]).

% Examples:
binary(z_add,z). % additin over integers
binary(z_sub,z). % subtraction over integers

z_add. z_sub. z.
associative(z_add).
func(z_add).
func(z_sub).
set(z).

test_unary(Pred,Atom) :- relation(1,Pred), call(Pred,Atom).

has_group_inverses(z_add).
group_identity(z_add,0).

% applying binary function --- binapp(Func,X,Y,Z) Z => Z = F(X,Y)
binapp(z_add,X,Y,X+Y).
binapp(z_sub,X,Y,X-Y).

% Definition 2
group(F):-
    binary(F,_),          % F is a binary operation on some set
    associative(F),       % F is associative
    group_identity(F,_),  % there exists a group identity (forall x: e*x = x = x*e)
    has_group_inverses(F).% forall x: exists x_inv: x_inv * x = 1


fact("Prop1.1",18,
     "The identity of G is unique",
     [group(_G)]).
fact("Prop1.2",18,
     "For each a in G, a^-1 is uniquely determined",
     [group(_G)]).
fact("Prop1.3",18,
     "(a^-1)^-1=a for all a in G",
     [group(_G)]).

fact("Prop2.1",20,
     "If au=av, then u=v",
     f21pair).
