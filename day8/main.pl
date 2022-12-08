:- use_module(library(between)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(iso_ext)).

star(1, X) :-
    phrase_from_file(tree_map(Map, Width), "input"),!,
    same_length(Map, Visible),
    map_visible(Map, Visible, 0, Width),
    sum_list(Visible, X).

star(2, X) :-
    phrase_from_file(tree_map(Map, Width), "input"),!,
    same_length(Map, Score),
    map_score(Map, Score, 0, Width),
    list_max(Score, X).

% PART 2
map_score(_, _, N, Width) :-
    N is Width*Width.

map_score(Map, Score, N, Width) :-
    nth0(N, Map, Tree),
    left_trees(Map, N, Width, LeftTrees),
    right_trees(Map, N, Width, RightTrees),
    up_trees(Map, N, Width, UpTrees),
    down_trees(Map, N, Width, DownTrees),
    reverse(LeftTrees, LeftTreesR),
    reverse(UpTrees, UpTreesR),
    maplist(count_trees(Tree), [LeftTreesR, RightTrees, UpTreesR, DownTrees], [S0, S1, S2, S3]),
    S is S0*S1*S2*S3,
    nth0(N, Score, S),
    N1 is N + 1,
    map_score(Map, Score, N1, Width).

count_trees(_, [], 0).
count_trees(T, [X|Xs], N) :-
    T > X,
    count_trees(T, Xs, N0),
    N is N0 + 1.
count_trees(T, [X|_], 1) :-
    X >= T.

% PART 1

map_visible(_, _, N, Width) :-
    N is Width*Width.

map_visible(Map, Visible, N, Width) :-
    nth0(N, Map, Tree),
    nth0(N, Visible, 1),
    (
	(
	    left_trees(Map, N, Width, LeftTrees),
	    forall(member(T, LeftTrees), T < Tree)
	) ;
	(
	    right_trees(Map, N, Width, RightTrees),
	    forall(member(T, RightTrees), T < Tree)
	) ;
	(
	    up_trees(Map, N, Width, UpTrees),
	    forall(member(T, UpTrees), T < Tree)
	) ;
	(
	    down_trees(Map, N, Width, DownTrees),
	    forall(member(T, DownTrees), T < Tree)
	)
    ),!,
    N1 is N + 1,
    map_visible(Map, Visible, N1, Width).

map_visible(Map, Visible, N, Width) :-
    nth0(N, Visible, 0),!,
    N1 is N + 1,
    map_visible(Map, Visible, N1, Width).    

left_trees(Map, N, Width, LeftTrees) :-
    findall(T, (
		Left is (N // Width) * Width,
		between(Left, N, NT),
		NT \= N,
		nth0(NT, Map, T)),
	    LeftTrees).

right_trees(Map, N, Width, Trees) :-
    findall(T, (
		Right is (((N // Width) + 1) * Width) - 1,
		between(N, Right, NT),
		NT \= N,
		nth0(NT, Map, T)),
	    Trees).

up_trees(Map, N, Width, Trees) :-
    findall(T, (
		between(0, N, NT),
		NT mod Width =:= N mod Width,
		NT \= N,
		nth0(NT, Map, T)),
	    Trees).

down_trees(Map, N, Width, Trees) :-
    findall(T, (
		Max is Width * Width,
		between(N, Max, NT),
		NT mod Width =:= N mod Width,
		NT \= N,
		nth0(NT, Map, T)),
	    Trees).

tree_map([], _) --> [].
tree_map(Xs, Width) -->
    tree_line(X),
    tree_map(Xs0, _), { length(X, Width), append(X, Xs0, Xs) }.

tree_line([]) --> "\n".
tree_line([X|Xs]) -->
    [N], { number_chars(X, [N]) },
    tree_line(Xs).
