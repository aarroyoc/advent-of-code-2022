:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(lists)).

star(1, X) :-
    phrase_from_file(code(Code), "input"),
    code_cycle(Code, CycleCode),
    execute(CycleCode, 1, 1, 0, SignalStrength),
    X = SignalStrength.

star(2, CRT) :-
    phrase_from_file(code(Code), "input"),
    code_cycle(Code, CycleCode),
    executeCRT(CycleCode, 0, 1, [], CRT),
    show_crt(CRT).

% PART 2

show_crt([]).
show_crt([X|Xs]) :-
    format("~a", [X]),
    length(Xs, N),
    Z is N mod 40,
    (
	Z = 0 ->
	format("~n", [])
    ;   true
    ),
    show_crt(Xs).

executeCRT([], _, _, CRT, CRT).
executeCRT([Ins|Xs], N, X, CRT0, CRT) :-
    (
	( Ins = noop, X1 = X)
    ;   ( Ins = addx1(_), X1 = X)
    ;   ( Ins = addx2(I), X1 is X + I)
    ),
    N1 is N + 1,
    crt(N, X, CRT0, CRT1), 
    executeCRT(Xs, N1, X1, CRT1, CRT).

crt(N, X, CRT0, CRT) :-
    Column is N mod 40,
    Z is abs(X - Column),
    (
	(Z = 0; Z = 1) ->
	append(CRT0, ['#'], CRT)
    ;   append(CRT0, ['.'], CRT)
    ).

% PART 1

execute([], _, _, S, S).
execute([Ins|Xs], N, X, SignalStrength0, SignalStrength) :-
    (
	( Ins = noop, X1 = X)
    ;   ( Ins = addx1(_), X1 = X)
    ;   ( Ins = addx2(I), X1 is X + I)
    ),
    N1 is N + 1,
    signal_strength(N1, X1, SignalStrength0, SignalStrength1),
    execute(Xs, N1, X1, SignalStrength1, SignalStrength).

signal_strength(N1, X1, S0, S) :-
    (N1 = 20; N1 = 60; N1 = 100; N1 = 140; N1 = 180; N1 = 220),
    S is S0 + (N1*X1).

signal_strength(_, _, S, S).
    
    
code_cycle([], []).
code_cycle([noop|Xs], [noop|Ys]) :-
    code_cycle(Xs, Ys).
code_cycle([addx(I)|Xs], [addx1(I),addx2(I)|Ys]) :-
    code_cycle(Xs, Ys).

code([]) --> [].
code([noop|Cs]) -->
    "noop\n",
    code(Cs).
code([addx(I)|Cs]) -->
    "addx ",
    seq(N),
    "\n",
    { number_chars(I, N) },
    code(Cs).
