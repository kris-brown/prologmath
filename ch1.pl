:- module(ch1,[type/2,binfunc/0]).

:- use_module(fact).
:- use_module(simple).

:- multifile fact:fact/5.
:- multifile simple:set/1.
:- multifile simple:func/1.
:- multifile simple:z_add/0.
:- multifile simple:z/0.

/*
Introduction to Groups
*/

%%%%%%%%%%%%%%%%%
% Declare atoms %
%%%%%%%%%%%%%%%%%

binfunc.

%%%%%%%%%%%%%%
% Predicates %
%%%%%%%%%%%%%%%
type(simple:z_add,[simple:z,simple:z,simple:z]).

%%%%%%%%%
% Facts %
%%%%%%%%%

fact:fact("Binary function",16,[Func,Set],"binary",
          "Consider Func as a binary function over Set") :-
          simple:func(Func),
          simple:set(Set),
          type(Func,[Set,Set,Set]).
