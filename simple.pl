:- module(simple,[
  z/0,n/0,      % sets
  z_add/0,      % functions
  even/1,odd/1,  % other predicates
  set/1,func/1,int/1,nat/1 % types of mathematical objects (predicates)
  ]).

:- use_module(fact).
:- multifile fact:fact/5.

/*
--------------------------------------------------------------------------------
Some basic math stuff that isn't explicitly covered in textbook.

Also, silly examples to demonstrate the fact infrastructure
--------------------------------------------------------------------------------
*/

%%%%%%%%%%%%%%%%%%%%%
% Term declarations %
%%%%%%%%%%%%%%%%%%%%%
z.     % set of integers
n.     % set of natural numbers
set.   % mathematical set
func.  % any kind of function (arrow type)
z_add. % addition function Z -> Z -> Z

%%%%%%%%%%%%%%%%%%%%
% "Types", sort of %
%%%%%%%%%%%%%%%%%%%%

% Sets
set(z). set(n).
% Funcs
func(z_add).
% Ints
int(1). % predicate must be inhabited explicitly or else '_' will not match?
int(X) :- integer(X).
% Nats
nat(0).
nat(X):- int(X), X >= 0.

%%%%%%%%%%%%%%%%%%%%
% Other predicates %
%%%%%%%%%%%%%%%%%%%%
even(X) :- 0 is X mod 2.
odd(X)  :- 1 is X mod 2.

%%%%%%%%%
% Facts %
%%%%%%%%%

fact:fact("Existence",44,[X],Y,
     "If you have an even integer, then there exists an integer N/2") :-
  int(X), even(X),
  int(Y).

fact:fact("NotPrime",45,[X],"prime",% don't pattern match on result - we didn't "construct" anything
          "Even natural number is prime iff it is equal to 2") :-
  nat(X), even(X).

fact:fact("OddSum",22,[Even,Odd],Sum,
          "Sum of odd and even number is odd") :-
 int(Even), int(Odd), int(Sum),
 even(Even),    odd(Odd),
 odd(Sum).

fact:fact("Between",22,[Lo,Hi],Mid,
          "Given two integers that differ by two, there is an int between them") :-
  int(Lo), int(Hi), % need to typecheck inputs so that we don't get
  Lo is Hi-2,               % type-related errors trying to evaluate
  int(Mid).
  
fact:fact("Constructions from two sets",342,[S1,S2],S3,
    "Given any two sets, one can construct the UNION set and INTERSECTION set") :-
    set(S1),set(S2),set(S3). % matches any two sets
