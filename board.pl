:- use_module(library(lists)).
:- dynamic last_dropped_marble/2.
:-
player(Color,MarblesOnBoard).
% Define a predicate to represent marbles on the board for a player.
% Arguments: Player (color/identifier), MarblesonBoard (list of (Row, Column) pairs)

board(7, [
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        
]).
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
    apply_momentum(Player,Row,Column).



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
    has_won_game(player1, Board),!, 
    Winner = player1.
is_terminal_state(Board, Winner) :-
    has_won_game(player2, Board),!,
    Winner = player2.

% Predicate to check if the game is in a final state.
% Arguments: Board, Winner

init_empty_board(Size, Board):-
    board(Size,Board).

% Predicate to initialize an empty board
% Arguments: Size, Board

can_move_marble(Row, Column, NewRow, NewColumn) :-
    within_bounderies(NewRow, NewColumn),            
    \+ is_marble_at(NewRow, NewColumn). 

% Predicate to check if a marble can be moved to a certain position
% Arguments: Row, Column, NewRow, NewColumn

adjacent_position(Row, Column, NewRow, NewColumn) :-
    (NewRow is Row - 1, NewColumn is Column);   
    (NewRow is Row + 1, NewColumn is Column);   
    (NewRow is Row, NewColumn is Column - 1);   
    (NewRow is Row, NewColumn is Column + 1);    
    (NewRow is Row - 1, NewColumn is Column - 1); 
    (NewRow is Row - 1, NewColumn is Column + 1); 
    (NewRow is Row + 1, NewColumn is Column - 1); 
    (NewRow is Row + 1, NewColumn is Column + 1). 

% Predicate to check if a position is adjacent to another
% Arguments: Row, Column, NewRow, NewColumn

is_adjacent_dropped_marble(Row, Column, NewRow, NewColumn) :-
    adjacent_position(Row, Column, NewRow, NewColumn),  
    is_marble_at(NewRow, NewColumn),                   
    last_dropped_marble(NewRow, NewColumn). 

% Predicate to check if a marble is adjacent to a just dropped marble
% Arguments: Row, Column, NewRow, NewColumn

get_opposite_direction(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn) :-
    OppositeRow is 2 * LastRow - Row,
    OppositeColumn is 2 * LastColumn - Column.

% Predicate to get the momentum direction
% Arguments: LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn

apply_momentum(Player, Row, Column) :-
    last_dropped_marble(LastRow, LastColumn),
    get_opposite_direction(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn),
    apply_momentum_to_directions(Player, OppositeRow, OppositeColumn). % Incompleto

% Predicate to apply the momentum
% Arguments: Player, Row, Column

apply_momentum_to_directions(_, _, _).
apply_momentum_to_directions(Player, Row, Column) :-
     is_adjacent_dropped_marble(Row, Column, NewRow, NewColumn),
     can_move_marble(NewRow, NewColumn, Row, Column),
     transfer(Player, Row, Column, NewRow, NewColumn), %Incompleto
     apply_momentum_to_directions(Player, Row, Column).


% Predicate to apply the momentum to the directions
% Arguments: Player, Row, Column

set_last_dropped_marble(Row, Column) :-
    retractall(last_dropped_marble(_, _)), 
    asserta(last_dropped_marble(Row, Column)).

% Predicate to set the last dropped marble
% Arguments: Row, Column

last_dropped_marble(Row, Column) :-
    last_dropped_marble(Row, Column).

% Predicate to check if a marble at a specific position was the latest dropped
% Arguments: Row, Column

