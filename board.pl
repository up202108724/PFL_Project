:- use_module(library(lists)).
:-consult
empty_board(7, 7, Board):-
    length(Board, 7),
    maplist(empty_row, Board).

empty_row(Row):-
    length(Row, 7),
    maplist(=(empty), Row).
empty_board([empty|Rest]):- empty_board([Rest]).

player(Color, MarblesInHand, MarblesOnBoard).
% Define a predicate to represent marbles on the board for a player.
% Arguments: Player (color/identifier), MarblesonBoard (list of (Row, Column) pairs)

within_bounderies(Row,Column):-
    Row >=1 ,
    Row =<7,
    Column >=1,
    Column =<7.

% Predicate to check if a marble is within the board bounderies
% Arguments: Row, Column

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



is_marble_at(Row,Column):-
   (is_playermarble_at('Red', Row, Column);is_playermarble_at('Blue', Row, Column)).

% Predicate to check if a marble is at a certain position
% Arguments: Row, Column

transfer(Player, Row, Column, Direction, NewRow, NewColumn):-
    within_bounderies(NewRow,NewColumn),
    is_playermarble_at(Player,Row,Column),
    \+is_marble_at(NewRow,NewColumn),
    marbles_on_board(Player,MarblesInHand),
    reverse(MarblesOnBoard, ReversedMarbles),
    select((Row, Column), ReversedMarbles, TempMarblesOnBoard),
    append(TempMarblesOnBoard, [(NewRow, NewColumn)], NewMarblesOnBoard),
    retract(marbles_on_board(Player, MarblesOnBoard)),
    assert(marbles_on_board(Player, NewMarblesOnBoard)).

% Predicate to transfer a marble from one position to another 
% Arguments: Player, Row, Column, Direction 
    
has_won_game(Player, Board) :-
    marbles_on_board(Player, MarblesOnBoard),
    MarblesOnBoard = 8.

% Predicate to check if a player has won the game
% Arguments: Player, Board

is_terminal_state(Board, Winner) :-
    has_won_game(player1, Board),
    Winner = player1.
is_terminal_state(Board, Winner) :-
    has_won_game(player2, Board),
    Winner = player2.

% Predicate to check if the game is in a final state.
% Arguments: Board, Winner

