:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(pio)).
:- use_module(library(lists)).

star(1, X) :-
    phrase_from_file(lines(Lines), "input"),
    phrase(cli(Commands), Lines),
    commands_tree(Commands, Tree),!,
    tree_size(Tree, _TreeSize),
    dirs_at_most_100000(Tree, Dirs),
    sum_list(Dirs, X).

star(2, X) :-
    phrase_from_file(lines(Lines), "input"),
    phrase(cli(Commands), Lines),
    commands_tree(Commands, Tree),!,
    tree_size(Tree, TreeSize),
    LiberateSpace is 30000000 - (70000000 - TreeSize),
    dirs_more_than_liberate(LiberateSpace, Tree, Dirs),
    list_min(Dirs, X).

dirs_more_than_liberate(_, [], []).
dirs_more_than_liberate(Space, [file(_, _)|Fs], Ls) :-
    dirs_more_than_liberate(Space, Fs, Ls).
dirs_more_than_liberate(Space, [dir(Name, Sub, Size)|Fs], Ls) :-
    Size > Space,
    dirs_more_than_liberate(Space, Sub, Ls0),
    dirs_more_than_liberate(Space, Fs, Ls1),    
    append(Ls0, [Size|Ls1], Ls).

dirs_more_than_liberate(Space, [dir(Name, Sub, Size)|Fs], Ls) :-
    Size =< Space,
    dirs_more_than_liberate(Space, Sub, Ls0),
    dirs_more_than_liberate(Space, Fs, Ls1),
    append(Ls0, Ls1, Ls).
    
dirs_at_most_100000([], []).
dirs_at_most_100000([file(_, _)|Fs], Ls) :-
    dirs_at_most_100000(Fs, Ls).
dirs_at_most_100000([dir(Name, Sub, Size)|Fs], Ls) :-
    Size =< 100000,
    dirs_at_most_100000(Sub, Ls0),
    dirs_at_most_100000(Fs, Ls1),    
    append(Ls0, [Size|Ls1], Ls).

dirs_at_most_100000([dir(Name, Sub, Size)|Fs], Ls) :-
    Size > 100000,
    dirs_at_most_100000(Sub, Ls0),
    dirs_at_most_100000(Fs, Ls1),
    append(Ls0, Ls1, Ls).


tree_size([], 0).
tree_size([file(_, Size0)|Fs], Size) :-
    tree_size(Fs, Size1),
    Size is Size0 + Size1.
tree_size([dir(Name, Sub, SubSize)|Fs], Size) :-
    tree_size(Sub, SubSize),
    tree_size(Fs, Size1),
    Size is SubSize + Size1.

commands_tree(Commands, Tree) :-
    commands_tree_(Commands, _CurrentFolder, Tree).

commands_tree_([], _, _).
commands_tree_([cd("/"), ls(Tree)|Cs], _, Tree) :-
    commands_tree_(Cs, [], Tree).

commands_tree_([cd("..")|Cs], CurrentFolder, Tree) :-
    append(UpFolder, [_], CurrentFolder),
    commands_tree_(Cs, UpFolder, Tree).

commands_tree_([cd(Folder)|Cs], CurrentFolder, Tree) :-
    append(CurrentFolder, [Folder], NewFolder),
    commands_tree_(Cs, NewFolder, Tree).

commands_tree_([ls(Output)|Cs], CurrentFolder, Tree) :-
    folder(CurrentFolder, Tree, Output),
    commands_tree_(Cs, CurrentFolder, Tree).

folder([N], Tree, X) :-
    memberchk(dir(N, X, _S), Tree).
folder([N|Ns], Tree, X) :-
    memberchk(dir(N, Sub, _S), Tree),
    folder(Ns, Sub, X).

lines([]) --> [].
lines([X|Xs]) -->
    seq(X),
    "\n",
    lines(Xs).

cli([]) --> [].
cli([cd(Folder)|Cs]) -->
    [Line],
    {
	phrase(("$ cd ", seq(Folder)), Line)
    },
    cli(Cs).
cli([ls(Output)|Cs]) -->
    [Line],
    {
	Line = "$ ls"
    },
    ls_output(Output),
    cli(Cs).

ls_output([]) --> [].
ls_output([file(Name, Size)|Xs]) -->
    [Line],
    {
	phrase((seq(SizeStr), " ", seq(Name)), Line),
	SizeStr \= "dir",
	number_chars(Size, SizeStr)
    },
    ls_output(Xs).
ls_output([dir(Name, _Contents, _Size)|Xs]) -->
    [Line],
    {
	phrase(("dir ", seq(Name)), Line)
    },
    ls_output(Xs).
    
