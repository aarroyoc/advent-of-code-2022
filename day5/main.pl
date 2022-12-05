:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).

star(1, X) :-
    phrase_from_file(data(StartState, Code), "input"),
    execute(Code, StartState, EndState),
    top_stack(EndState, X).

star(2, X) :-
    phrase_from_file(data(StartState, Code), "input"),
    execute9001(Code, StartState, EndState),
    top_stack(EndState, X).

% PART 2

execute9001([], State, State).
execute9001([Instruction|Is], StartState, EndState) :-
    Instruction = move(N, From, To),
    move9001(N, From, To, StartState, State),
    execute9001(Is, State, EndState).

move9001(N, From, To, S0, S2) :-
    remove_n_elements(N, Elements, From, S0, S1),
    add_elements(Elements, To, S1, S2).

remove_n_elements(N, Elements, From, S0, S) :-
    Ls1Length is From - 1,
    length(Ls1, Ls1Length),
    append(Ls1, [L|Ls2], S0),
    length(Elements, N),
    append(Elements, Ls, L),
    append(Ls1, [Ls|Ls2], S).

add_elements(Elements, To, S0, S) :-
    Ls1Length is To - 1,
    length(Ls1, Ls1Length),
    append(Ls1, [L|Ls2], S0),
    append(Elements, L, NewL),
    append(Ls1, [NewL|Ls2], S).

% PART 1

top_stack(S, Top) :-
    maplist(head, S, Top).

head([X|_], X).

execute([], State, State).
execute([Instruction|Is], StartState, EndState) :-
    Instruction = move(N, From, To),
    move(N, From, To, StartState, State),
    execute(Is, State, EndState).

move(0, _, _, S, S).
move(N0, From, To, S0, S) :-
    N0 > 0,
    N is N0 - 1,
    remove_element(Element, From, S0, S1),
    add_element(Element, To, S1, S2),
    move(N, From, To, S2, S).

nth(N0, L, E) :-
    N is N0 + 1,
    nth0(N, L, E).

remove_element(Element, N, S0, S1) :-
    Ls1Length is N - 1,
    length(Ls1, Ls1Length),
    append(Ls1, [L|Ls2], S0),
    L = [Element|Ls],
    append(Ls1, [Ls|Ls2], S1).

add_element(Element, N, S0, S1) :-
    Ls1Length is N - 1,
    length(Ls1, Ls1Length),
    append(Ls1, [L|Ls2], S0),
    NewL = [Element|L],
    append(Ls1, [NewL|Ls2], S1).

data(StartState, Code) -->
    state(StartState),
    ... ,
    "\n\n",
    code(Code).

state(["WRTG", "WVSMNPHCG", "MGSTLC", "FRWMDHJ", "JFWSHLQP", "SMFNDJP", "JSCGFDBZ", "BTR", "CLWNH"]) --> ... .
% state(["NZ", "DCM", "P"]) -->  ... .

code([]) --> [].
code([move(N, From, To)|Ms]) -->
    "move ",
    seq(NStr),
    " from ",
    seq(FromStr),
    " to ",
    seq(ToStr),
    "\n",
    {
	number_chars(N, NStr),
	number_chars(From, FromStr),
	number_chars(To, ToStr)
    },
    code(Ms).
