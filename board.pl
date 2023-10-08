:- use_module(library(lists)).
empty_board(7,7,Board):-
    length(Board,7),
    maplist(same_length(Board), Board).
    maplist(empty_row, Board).
empty_row([]).
empty_board([empty|Rest]):- empty_board([Rest]).

player(Color, MarblesInHand, MarblesOnBoard).
% Define a predicate to represent marbles on the board for a player.
% Arguments: Player (color/identifier), MarblesonBoard (list of (Row, Column) pairs)

within_bounderies(Row,Column):-
    Row >=1 , 
    Row <=7,
    Column >=1, 
    Column <=7.

marbles_on_board(Player, MarblesonBoard).

% Predicate to place a marble on the board
% Arguments: Player, Row, Column

place_marble(Player,Row,Column):-
    within_bounderies(Row,Column),
    \+ is_marble_at(Row,Column),
    marbles_on_board(Player,MarblesonBoard),
    append(marbles_on_board,[(Row,Column)], NewMarblesonBoard),
    retract(marbles_on_board(Player,MarblesonBoard)),
    assert(marbles_on_board(Player,NewMarblesonBoard)).

is_playermarble_at(Player, Row, Column):-
    marbles_on_board(Player, MarblesOnBoard),
    member([(Row,Column)], MarblesOnBoard).

% Predicate to check if a specific (Row, Column) pair is already occupied by a marble for either player
% Arguments: Row, Column

is_marble_at(Row,Column):-
   (is_playermarble_at('Red', Row, Column);is_playermarble_at('Blue', Row, Column));          

transfer(Player, Row, Column, Direction):-
    within_bounderies(NewRow,NewColumn),
    is_playermarble_at(Player,Row,Column),
    \+is_marble_at(NewRow,NewColumn),
    marbles_on_board(Player,MarblesInHand),
    reverse(MarblesOnBoard, ReversedMarbles),
    select((Row, Column), ReversedMarbles, TempMarblesOnBoard),
    append(TempMarblesOnBoard, [(NewRow, NewColumn)], NewMarblesOnBoard),
    retract(marbles_on_board(Player, MarblesOnBoard)),
    assert(marbles_on_board(Player, NewMarblesOnBoard)).