:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(pio)).

star(1, X) :-
    phrase_from_file(rucksacks(Rucksacks), "input"),
    maplist(common_items, Rucksacks, CommonItems),
    maplist(items_priority, CommonItems, Priorities),
    sum_list(Priorities, X).

star(2, X) :-
    phrase_from_file(rucksacks(Rucksacks), "input"),
    maplist(rucksacks_join, Rucksacks, JoinedRucksacks),
    common_group_items(JoinedRucksacks, CommonItems),
    maplist(items_priority, CommonItems, Priorities),
    sum_list(Priorities, X).

% PART 2

rucksacks_join(rucksack(P1, P2), P) :-
    append(P1, P2, P).

common_group_items([], []).
common_group_items([P1,P2,P3|Rs], [C|Cs]) :-
    member(C, P1),
    member(C, P2),
    member(C, P3),
    common_group_items(Rs, Cs).

% PART 1

items_priority(Item, Priority) :-
    P = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    nth0(N, P, Item),
    Priority is N + 1.

common_items(rucksack(P1, P2), Common) :-
    member(Common, P1),
    member(Common, P2).

rucksacks([]) --> [].
rucksacks([rucksack(P1, P2)|Rs]) -->
    seq(Xs),
    "\n",
    {
	append(P1, P2, Xs),
	length(P1, N),
	length(P2, N)
    },
    rucksacks(Rs).
