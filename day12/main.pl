:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(between)).

star(1, X) :-
    phrase_from_file(lines(Lines, Width), "input"),!,
    length(Lines, Height),
    append(Lines, MapData),
    Map = map(MapData, Width, Height),
    start_pos(Map, StartPos),
    end_pos(Map, EndPos),
    solve([0-StartPos], [], Map, EndPos, Steps),
    X = Steps.

% PART 2

star(2, Steps) :-
    phrase_from_file(lines(Lines, Width), "input"), !,
    length(Lines, Height),
    append(Lines, MapData),
    Map = map(MapData, Width, Height),
    findall(0-Pos, start_pos_2(Map, Pos), StartPos),
    end_pos(Map, EndPos),
    solve(StartPos, [], Map, EndPos, Steps).

start_pos_2(map(MapData, Width, Height), Pos) :-
    Max is Width*Height - 1,
    between(0, Max, N),
    ( nth0(N, MapData, 'a') ; nth0(N, MapData, 'S')),
    X is N mod Width,
    Y is N // Width,
    Pos = X-Y.

% PART 1

solve([Steps-(X-Y)|_], _, _, X-Y, Steps).
solve([Steps-Pos0|RestMoves], Visited, Map, End, FinalSteps) :-
    findall(NewSteps-NewPos, (
		NewSteps is Steps + 1,
		move(_Move, Pos0, Map, NewPos),
		\+ member(NewPos, Visited),
		\+ member(_-NewPos, RestMoves)
	    ), NewMoves0),
    append(NewMoves0, RestMoves, NewMoves),
    keysort(NewMoves, Moves0),
    solve(Moves0, [Pos0|Visited], Map, End, FinalSteps).

elevation(A0, B0) :-
    (
	A0 = 'S' ->
	A1 = 'a'
    ;   A1 = A0
    ),
    (
	B0 = 'E' ->
	B1 = 'z'
    ;   B1 = B0
    ),
    X = "abcdefghijklmnopqrstuvwxyz",
    nth0(X0, X, A1),
    nth0(X1, X, B1),
    X0 + 1 >= X1.

move(up, X0-Y0, Map, X1-Y1) :-
    X0 = X1,
    Y1 is Y0 + 1,
    pos(Map, X0-Y0, N0),
    pos(Map, X1-Y1, N1),
    elevation(N0, N1).

move(down, X0-Y0, Map, X1-Y1) :-
    X0 = X1,
    Y1 is Y0 - 1,
    pos(Map, X0-Y0, N0),
    pos(Map, X1-Y1, N1),
    elevation(N0, N1).

move(left, X0-Y0, Map, X1-Y1) :-
    X1 is X0 - 1,
    Y1 = Y0,
    pos(Map, X0-Y0, N0),
    pos(Map, X1-Y1, N1),
    elevation(N0, N1).

move(right, X0-Y0, Map, X1-Y1) :-
    X1 is X0 + 1,
    Y1 = Y0,
    pos(Map, X0-Y0, N0),
    pos(Map, X1-Y1, N1),
    elevation(N0, N1).

pos(map(Map, Width, Height), X-Y, Value) :-
    X >= 0,
    Y >= 0,
    X < Width,
    Y < Height,
    N is X + Y*Width,
    nth0(N, Map, Value).

start_pos(map(Map, Width, _), X-Y) :-
    nth0(N, Map, 'S'),
    X is N mod Width,
    Y is N // Width.

end_pos(map(Map, Width, _), X-Y) :-
    nth0(N, Map, 'E'),
    X is N mod Width,
    Y is N // Width.

lines([], _) --> [].
lines([X|Xs], Width) -->
    seq(X),
    "\n",
    { length(X, Width) },
    lines(Xs, Width).
