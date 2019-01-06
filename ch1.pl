:- module(ch1,[type/2,binfunc/0]).
:- use_module(fact).
/*
Introduction to Groups
*/
:- multifile fact:pattern/3.
:- multifile fact:fact/4.
:- multifile fact:z_add/0.
:- multifile fact:func/1.
:- multifile fact:set/1.

%%%%%%%%%%%%%%%%%
% Declare atoms %
%%%%%%%%%%%%%%%%%

binfunc.

%%%%%%%%%%%%
% Patterns %
%%%%%%%%%%%%

% fact:pattern(binfunc,2,[Func,Set]) :-
%   func(Func),
%   set(Set),
%   type(Func,[Set,Set,Set]).
%
%%%%%%%%%
% Facts %
%%%%%%%%%

fact:fact("Binary function",16,[Func,Set],
          "Consider Func as a binary function over Set") :-
          func(Func),
          set(Set),
          type(Func,[Set,Set,Set]).

type(z_add,[z,z,z]).
