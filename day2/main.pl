:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).

star(1, X) :-
    data(Data, "input"),
    play(Data, X).

star(2, X) :-
    data(Data, "input"),
    play_two(Data, X).

% PART 1

shape_score('X', 1).
shape_score('Y', 2).
shape_score('Z', 3).

outcome_score('A', 'X', 3).
outcome_score('A', 'Y', 6).
outcome_score('A', 'Z', 0).
outcome_score('B', 'X', 0).
outcome_score('B', 'Y', 3).
outcome_score('B', 'Z', 6).
outcome_score('C', 'X', 6).
outcome_score('C', 'Y', 0).
outcome_score('C', 'Z', 3).

play([], 0).
play([round(X, Y)|Rs], Score) :-
    outcome_score(X, Y, Score0),
    shape_score(Y, Score1),
    play(Rs, ScoreRs),
    Score is Score0 + Score1 + ScoreRs.

% PART 2

outcome_shape_score('X', 0).
outcome_shape_score('Y', 3).
outcome_shape_score('Z', 6).

outcome_shape(X, Y, Z) :-
    outcome_shape_score(Y, P),
    outcome_score(X, Z, P). 

play_two([], 0).
play_two([round(X, Y)|Rs], Score) :-
    outcome_shape(X, Y, Z),
    outcome_shape_score(Y, Score0),
    shape_score(Z, Score1),
    play_two(Rs, ScoreRs),
    Score is Score0 + Score1 + ScoreRs.
    
% DATA
    
data(Data, File) :-
    phrase_from_file(strategy_guide(Data), File).

strategy_guide([]) --> [].
strategy_guide([round(X, Y)|Xs]) -->
    [X],
    " ",
    [Y],
    "\n",
    strategy_guide(Xs).
    
