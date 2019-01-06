:-module(base,
  [nil/0,n/0,z/0,                   %sets
    z_sub/0,z_add/0,z_mul/0,z_div/0,% funcs
    set/1,func/1,                   % predicates
    elements/2,elem/2,types/2       % set-related
    ]).

% Sets
nil. n. z.
set(nil). % empty set
set(n).   % natural numbers
set(z).   % integers

elements(nil,[]). % empty set has no elements

elem(Element,Set) :-
  elements(Set,Y),member(Element,Y).

% Special checks for membership in named infinite sets
elem(Element,z) :- integer(Element).
elem(Element,n) :- integer(Element),Element >= 0.

z_sub. z_add. z_mul. z_div. add1.

func_class(add1,[z,z],app(X,1+X)).

func_class(z_sub,[z,z,z]).
func_class(z_add,[z,z,z]).
func_class(z_mul,[z,z,z]).
func_class(z_div,[z,z,z_no_zero]).


% Function stuff
func(Func) :- func_class(Func,_,_).
types(Func,Types) :- func_class(Func,Types,_). % actually it's just a synonym unless we add more to the class
funcapp(Func,App) :- func_class(Func,_,App).

% Evaluate function if only one term is remaining
eval(Func,Arg,Result) :-
    funcapp(Func,app(Arg,Result)).


%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCTION APPLICATION %
%%%%%%%%%%%%%%%%%%%%%%%%
% Applying a curried function which does not yield a term
% app(Func,X,Curry) :-
%   types(Func,[Intype,Nexttype|Remaining]), % func does not have simple X->Y (which would yield a term upon application)
%   append(Nexttype,Remaining,CurrType), % type of currried func
%   types(Curry,CurrType),
%   elem(X,Intype). % checked typedness
%
% % Applying a "simple" function which yields a term, rather than curried func
% app(Func,X,Y) :-
%   types(Func,[Intype,Outtype]),
%   elem(X,Intype),
%   elem(Y,Outtype).

% You can't 'apply' a function of just one type, zero also doesn't make sense


%%% Specific applications %%%
app(z_add,A,Curry) :-
  app(Curry,B,Sum),
  Sum is A+B.

app(z_add,1,add1).
