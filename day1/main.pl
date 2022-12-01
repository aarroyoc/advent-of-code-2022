:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).

star(1, X) :-
    phrase_from_file(lines(Lines), "input"),
    elf_data(Lines, Data),
    maplist(sum_list, Data, Elves),
    list_max(Elves, X).

star(2, X) :-
    phrase_from_file(lines(Lines), "input"),
    elf_data(Lines, Data),
    maplist(sum_list, Data, Elves),
    maplist(list_to_keysort, Elves, ElvesKeysort),
    keysort(ElvesKeysort, ElvesSorted), % sort removes duplicates
    reverse(ElvesSorted, ElvesSortedDescent),
    ElvesSortedDescent = [Elf1-a, Elf2-a, Elf3-a|_],
    sum_list([Elf1, Elf2, Elf3], X).

list_to_keysort(X, X-a).

lines([]) --> [].
lines([X|Xs]) --> seq(X), "\n", lines(Xs).

elf_data(Xs, Ys) :- elf_data_(Xs, [], Ys).
elf_data_([X|Xs], Zs, Ys) :-
    X \= "",
    number_chars(Z, X),
    elf_data_(Xs, [Z|Zs], Ys).

elf_data_([X|Xs], Zs, [Zs|Ys]) :-
    X = "",
    elf_data_(Xs, [], Ys).

elf_data_([], Zs, [Zs]).
