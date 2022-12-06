:- use_module(library(dif)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pio)).

star(1, X) :-
    phrase_from_file(seq(Data), "input"),
    first_marker(Data, 4, X).

star(2, X) :-
    phrase_from_file(seq(Data), "input"),
    first_message(Data, 14, X).

% PART 2
first_message(Data, Chars0, Chars) :-
    length(ListSet, 14),
    append(ListSet, Xs, Data),
    list_to_ord_set(ListSet, Set),
    (
	length(Set, 14) ->
	Chars0 = Chars
    ;   (
	Chars1 is Chars0 + 1,
	ListSet = [_|Ls],
	append(Ls, Xs, Data1),
	first_message(Data1, Chars1, Chars)
        )
    ).

% PART 1

first_marker([A,B,C,D|Xs], Chars0, Chars) :-
   ( dif(A, B),
     dif(B, C),
     dif(C, D),
     dif(A, C),
     dif(A, D),
     dif(B, D),
     Chars0 = Chars
   ) ; (
       Chars1 is Chars0 + 1,
       first_marker([B,C,D|Xs], Chars1, Chars)
   ).
       
