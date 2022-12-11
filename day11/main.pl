:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(format)).

star(1, X) :-
    monkey_data(Data0),
    round(20, Data0, Data),
    maplist(monkey_ins, Data, Ins),
    keysort(Ins, InsSort),
    append(_, [A-a, B-a], InsSort),
    X is A * B.

star(2, X) :-
    monkey_data(Data0),
    round2(10000, Data0, Data),
    maplist(monkey_ins, Data, Ins),
    keysort(Ins, InsSort),
    append(_, [A-a, B-a], InsSort),
    X is A * B.

% PART 2

round2(0) --> [].
round2(N) -->
    monkey2(0),
    monkey2(1),
    monkey2(2),
    monkey2(3),
    monkey2(4),
    monkey2(5),
    monkey2(6),
    monkey2(7),
    { N1 is N - 1 },
    round2(N1).

monkey2(N, D, D) :-
    member(monkey(N, [], _, _, _, _), D).
monkey2(N, D0, D) :-
    select(monkey(N, Items, Test, True, False, Ins0), D0, D1),
    Ins is Ins0 + 1,
    Items = [FirstItem|Is],
    monkey_op(N, FirstItem, New0),
    Z is New0 mod Test,
    (
	Z = 0 ->
	( NewMonkey = True )
    ;   ( NewMonkey = False )
    ),
    New is New0 mod (2*3*5*7*11*13*17*19*23),
    select(monkey(NewMonkey, ItemsNewMonkey, TestNewMonkey, TrueNewMonkey, FalseNewMonkey, InsNewMonkey), D1, D2),
    append(ItemsNewMonkey, [New], ItemsNewMonkey1),
    select(monkey(NewMonkey, ItemsNewMonkey1, TestNewMonkey, TrueNewMonkey, FalseNewMonkey, InsNewMonkey), D3, D2),
    select(monkey(N, Is, Test, True, False, Ins), D4, D3),
    monkey2(N, D4, D).

% PART 1

monkey_ins(monkey(_, _, _, _, _, Ins), Ins-a).

round(0) --> [].
round(N) -->
    monkey(0),
    monkey(1),
    monkey(2),
    monkey(3),
    monkey(4),
    monkey(5),
    monkey(6),
    monkey(7),
    { N1 is N - 1 },
    round(N1).

monkey(N, D, D) :-
    member(monkey(N, [], _, _, _, _), D).
monkey(N, D0, D) :-
    select(monkey(N, Items, Test, True, False, Ins0), D0, D1),
    Ins is Ins0 + 1,
    Items = [FirstItem|Is],
    monkey_op(N, FirstItem, NewItem),
    NewItem1 is NewItem // 3,
    Z is NewItem1 mod Test,
    (
	Z = 0 ->
	NewMonkey = True
    ;   NewMonkey = False
    ),
    select(monkey(NewMonkey, ItemsNewMonkey, TestNewMonkey, TrueNewMonkey, FalseNewMonkey, InsNewMonkey), D1, D2),
    append(ItemsNewMonkey, [NewItem1], ItemsNewMonkey1),
    select(monkey(NewMonkey, ItemsNewMonkey1, TestNewMonkey, TrueNewMonkey, FalseNewMonkey, InsNewMonkey), D3, D2),
    select(monkey(N, Is, Test, True, False, Ins), D4, D3),
    monkey(N, D4, D).

% monkey(MonkeyID, Items, Test divisible by, Throw if true, Throw if false, Inspections)
%% monkey_data([
%%     monkey(0, [79, 98], 23, 2, 3, 0),
%%     monkey(1, [54, 65, 75, 74], 19, 2, 0, 0),
%%     monkey(2, [79, 60, 97], 13, 1, 3, 0),
%%     monkey(3, [74], 17, 0, 1, 0)
%% ]).

%% monkey_op(0, Old, New) :- New is Old * 19.
%% monkey_op(1, Old, New) :- New is Old + 6.
%% monkey_op(2, Old, New) :- New is Old * Old.
%% monkey_op(3, Old, New) :- New is Old + 3.


monkey_data([
		   monkey(0, [89, 74], 17, 4, 7, 0),
		   monkey(1, [75, 69, 87, 57, 84, 90, 66, 50], 7, 3, 2, 0),
		   monkey(2, [55], 13, 0, 7, 0),
		   monkey(3, [69, 82, 69, 56, 68], 2, 0, 2, 0),
		   monkey(4, [72, 97, 50], 19, 6, 5, 0),
		   monkey(5, [90, 84, 56, 92, 91, 91], 3, 6, 1, 0),
		   monkey(6, [63, 93, 55, 53], 5, 3, 1, 0),
		   monkey(7, [50, 61, 52, 58, 86, 68, 97], 11, 5, 4, 0)
	       ]).
monkey_op(0, Old, New) :- New is Old * 5.
monkey_op(1, Old, New) :- New is Old + 3.
monkey_op(2, Old, New) :- New is Old + 7.
monkey_op(3, Old, New) :- New is Old + 5.
monkey_op(4, Old, New) :- New is Old + 2.
monkey_op(5, Old, New) :- New is Old * 19.
monkey_op(6, Old, New) :- New is Old * Old.
monkey_op(7, Old, New) :- New is Old + 4.
