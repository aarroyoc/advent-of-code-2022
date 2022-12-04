:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pio)).

star(1, X) :-
    phrase_from_file(pairs(Data), "input"),
    maplist(contains, Data, Contains),
    sum_list(Contains, X).

star(2, X) :-
    phrase_from_file(pairs(Data), "input"),
    maplist(overlaps, Data, Overlaps),
    sum_list(Overlaps, X).

% PART 2
overlaps(pair(range(S0, E0), range(S1, E1)), 1) :-
    S1 >= S0,
    S1 =< E0.

overlaps(pair(range(S0, E0), range(S1, E1)), 1) :-
    S0 >= S1,
    S0 =< E1.

overlaps(_, 0).

% PART 1

% R1 is in R0
contains(pair(range(S0, E0), range(S1, E1)), 1) :-
    S0 =< S1,
    E1 =< E0.

% R0 is in R1
contains(pair(range(S0, E0), range(S1, E1)), 1) :-
    S1 =< S0,
    E0 =< E1.

% No contains
contains(_, 0).

pairs([]) --> [].
pairs([pair(R0, R1)|Ps]) -->
    range(R0),
    ",",
    range(R1),
    "\n",
    pairs(Ps).

range(range(Start, End)) -->
    seq(StartStr),
    "-",
    seq(EndStr),
    {
	length(StartStr, N0),
	N0 > 0,
	length(EndStr, N1),
	N1 > 0,
	number_chars(Start, StartStr),
	number_chars(End, EndStr)
    }.
