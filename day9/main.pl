:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(lists)).

star(1, X) :-
    phrase_from_file(steps(Steps), "input"),!,
    steps_singlestep(Steps, FlattenSteps),!,
    execute(FlattenSteps, 0-0, [0-0], Visited),
    sort(Visited, Unique),
    length(Unique, X).

star(2, X) :-
    phrase_from_file(steps(Steps), "input"),!,
    steps_singlestep(Steps, FlattenSteps),!,
    execute2(FlattenSteps, rope(0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0), [0-0], Visited),
    sort(Visited, Unique),
    length(Unique, X).

% PART 2
execute2([], _, Visited, Visited).
execute2([Step|Rest], Rope, [TX-TY|Ts], Visited) :-
    Step = step(Dir, _),
    Rope = rope(HX-HY, AX-AY, BX-BY, CX-CY, DX-DY, EX-EY, FX-FY, GX-GY, IX-IY),
    (
	( Dir = 'R', HX1 is HX + 1, HY1 = HY )
    ;   ( Dir = 'L', HX1 is HX - 1, HY1 = HY )
    ;   ( Dir = 'U', HX1 = HX, HY1 is HY + 1 )
    ;   ( Dir = 'D', HX1 = HX, HY1 is HY - 1 )
    ),
    tail_position(HX1-HY1, AX-AY, AX1-AY1),
    tail_position(AX1-AY1, BX-BY, BX1-BY1),
    tail_position(BX1-BY1, CX-CY, CX1-CY1), 
    tail_position(CX1-CY1, DX-DY, DX1-DY1),
    tail_position(DX1-DY1, EX-EY, EX1-EY1),
    tail_position(EX1-EY1, FX-FY, FX1-FY1),
    tail_position(FX1-FY1, GX-GY, GX1-GY1),
    tail_position(GX1-GY1, IX-IY, IX1-IY1),
    tail_position(IX1-IY1, TX-TY, TX1-TY1),
    Rope1 = rope(HX1-HY1, AX1-AY1, BX1-BY1, CX1-CY1, DX1-DY1, EX1-EY1, FX1-FY1, GX1-GY1, IX1-IY1),
    execute2(Rest, Rope1, [TX1-TY1,TX-TY|Ts], Visited).

% PART 1

execute([], _, Visited, Visited).
execute([Step|Rest], HX-HY, [TX-TY|Ts], Visited) :-
    Step = step(Dir, _),
    (
	( Dir = 'R', HX1 is HX + 1, HY1 = HY )
    ;   ( Dir = 'L', HX1 is HX - 1, HY1 = HY )
    ;   ( Dir = 'U', HX1 = HX, HY1 is HY + 1 )
    ;   ( Dir = 'D', HX1 = HX, HY1 is HY - 1 )
    ),
    tail_position(HX1-HY1, TX-TY, TX1-TY1),
    execute(Rest, HX1-HY1, [TX1-TY1,TX-TY|Ts], Visited).

tail_position(HX-HY, TX-TY, TX-TY) :-
    1 >= abs(TX - HX),
    1 >= abs(TY - HY),!.

tail_position(HX-HY, TX-TY, TX1-TY1) :-
    (
	(TX > HX, TX1 is TX - 1)
    ;   (TX < HX, TX1 is TX + 1)
    ;   (TX1 = TX)
    ),
    (
	(TY > HY, TY1 is TY - 1)
    ;   (TY < HY, TY1 is TY + 1)
    ;   (TY1 = TY)
    ).

steps_singlestep([], []).
steps_singlestep([X|Xs], Ys) :-
    X = step(Direction, N),
    length(NewSteps, N),
    maplist(single_step(Direction), NewSteps),
    steps_singlestep(Xs, Ys0),
    append(NewSteps, Ys0, Ys).

single_step(Direction, step(Direction, 1)).

steps([]) --> [].
steps([step(Direction, N)|Xs]) -->
    [Direction],
    " ",
    seq(X),
    "\n",
    {
	number_chars(N, X)
    },
    steps(Xs).
