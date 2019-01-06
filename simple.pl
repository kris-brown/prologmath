:- module(simple,[
  z/0,n/0,      % sets
  z_add/0,      % functions
  set/1,func/1, % types of mathematical objects (predicates)
  even/1,odd/1  % other predicates
  ]).

:- use_module(fact).

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

%%%%%%%%%%%%%%%%%%%%
% Other predicates %
%%%%%%%%%%%%%%%%%%%%
even(X) :- 0 is X mod 2.
odd(X)  :- 1 is X mod 2.

%%%%%%%%%
% Facts %
%%%%%%%%%

fact:fact("Existence",44,[X],
     "If you have an even integer, then there exists an integer N/2") :-
  integer(X), even(X).

fact:fact("NotPrime",45,[X],
          "Even integer is prime iff it is equal to 2") :-
  integer(X), even(X).

fact:fact("OddSum",22,[Even,Odd],
          "Sum of odd and even number is odd") :-
 integer(Even), integer(Odd),
 even(Even),    odd(Odd).

fact:fact("Between",22,[Lo,Hi],
          "Given two integers that differ by two, there is a number in between
          them") :-
  integer(Lo), integer(Hi), % need to typecheck inputs so that we don't get
  Lo is Hi-2.               % type-related errors trying to evaluate

fact:fact("Constructions from two sets",342,[S1,S2],
    "Given any two sets, one can construct the UNION set and INTERSECTION set") :-
    set(S1),set(S2). % matches any two sets
