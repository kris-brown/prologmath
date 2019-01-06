:- module(ch1,
  [group/2,binary/2,types/2]).

:-use_module(base).

% Definition 1
binary(Func,Set) :-
  func(Func),
  set(Set),
  types(Func,[Set,Set,Set]).

binary(z_addition,z).

% Shorthand for applying binary function
binapp(Func,X,Y,Z) :-
    binary(Func,S),
    elem(X,S),elem(Y,S),
    app(Func,X,Curry),
    app(Curry,Y,Z).


associative(F) :-
  binary(F,S),
  elem(A,S),elem(B,S),elem(C,S), % arbitrary elements for specifying axioms
  binapp(F,binapp(F,A,B),C) = binapp(F,A,binapp(F,B,C)).


% Definition 2
group(F,S):-
    binary(F,S),
    associative(F),
    elem(Identity,S), % there exists an element called Identity
  binapp(F,A,Identity,A),
  binapp(F,Identity,A,A),
  elem(C,S), % this isn't correct, but for every elem in S there is an inverse
  binapp(F,_,C) = Identity.
