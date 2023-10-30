:- use_module(library(lists)).
:- dynamic last_dropped_marble/2.

:-dynamic marbles_on_board/1.

initialize_marbles(MarblesonBoard) :-
    assertz(marbles_on_board(MarblesonBoard)).
% Define a predicate to represent marbles on the board for a player.
% Arguments: Player (color/identifier), MarblesonBoard (list of (Row, Column) pairs)

board(7, [
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty]
        
]).
within_bounderies(Row,Column):-
    Row >=1 ,
    Row =<7,
    Column >=1,
    Column =<7.

% Predicate to check if a marble is within the board bounderies
% Arguments: Row, Column


% Predicate to place a marble on the board
% Arguments: Player, Row, Column

place_marble(Player, Row, Column):-
    marbles_on_board(MarblesOnBoard),
    within_boundaries(Row, Column),
    \+ is_marble_at(Player, Row, Column),
    NewMarble = (Player, Row, Column),
    append(MarblesOnBoard, [NewMarble], UpdatedMarblesOnBoard), 
    retract(marbles_on_board(_)), % Remove the old state
    assertz(marbles_on_board(UpdatedMarblesOnBoard)), 
    set_last_dropped_marble(Row, Column),
    retractall(adjacent_marbles(_)),
    adjacent_marbles(AdjacentMarbles),
    get_opposite_marbles_recursive,
    apply_momentum_to_adjacent_marbles(AdjacentMarbles).



is_marble_at(Player,Row, Column, MarblesOnBoard):-
    member([(Player,Row,Column)], MarblesOnBoard).


% Predicate to check if a marble is at a certain position
% Arguments: Row, Column

transfer(Row, Column, NewRow, NewColumn) :- % Temporario 
    within_bounderies(NewRow, NewColumn),
    marbles_on_board(MarblesOnBoard),
    is_marble_at(Player,Row, Column,MarblesOnBoard),
    \+ is_marble_at(_,NewRow, NewColumn,MarblesOnBoard),
    
    member((Player, Row, Column), MarblesonBoard),
    select((Player, Row, Column), MarblesonBoard, TempMarbles),
    retract(marbles_on_board(_)),
    NewMarblesOnBoard = [(Player, NewRow, NewColumn) | TempMarbles],
    assertz(marbles_on_board(NewMarblesOnBoard)).

% Predicate to transfer a marble from one position to another 
% Arguments: Player, Row, Column, Direction 
    
has_won_game(Player, MarblesOnBoard) :-
    findall((Player, _, _), member((Player, _, _), MarblesOnBoard), PlayerMarbles),
    length(PlayerMarbles, NumMarbles),
    NumMarbles >= 8.

% Predicate to check if a player has won the game
% Arguments: Player, Board

is_terminal_state(MarblesOnBoard, Winner) :-
    has_won_game(player1, MarblesOnBoard),
    !, % Cut here to stop backtracking
    Winner = player1.
is_terminal_state(MarblesOnBoard, Winner) :-
    has_won_game(player2, MarblesOnBoard),
    !, % Cut here to stop backtracking
    Winner = player2.
% Predicate to check if the game is in a final state.
% Arguments: Board, Winner

init_empty_board(Size, Board):-
    board(Size,Board).

% Predicate to initialize an empty board
% Arguments: Size, Board

can_move_marble( NewRow, NewColumn) :-
    within_bounderies(NewRow, NewColumn),
    \+ has_adjacent_marble(NewRow, NewColumn).

% Predicate to check if a marble can be moved to a certain position
% Arguments: Row, Column, NewRow, NewColumn

has_adjacent_marble(Row, Column,MarblesOnBoard) :-
    is_marble_at(Row, Column,MarblesOnBoard),
    apply_momentum(Row, Column).

% Predicate to check if a marble has an adjacent marble
% Arguments: Row, Column


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

is_adjacent_dropped_marble(Row, Column, LastRow, LastColumn) :-
    adjacent_position(Row, Column, LastRow, LastColumn),                    
    last_dropped_marble(LastRow, LastColumn). 

% Predicate to check if a marble is adjacent to a just dropped marble
% Arguments: Row, Column, NewRow, NewColumn

adjacent_marbles(AdjacentMarbles,MarblesOnBoard) :-
    marbles_on_board(MarblesOnBoard),
    last_dropped_marble(LastRow, LastColumn),
    findall((NewRow, NewColumn), 
            (adjacent_position(LastRow, LastColumn, NewRow, NewColumn), 
             is_marble_at(_,NewRow, NewColumn,MarblesOnBoard)),
            AdjacentMarbles).

% Predicate to get the adjacent marbles of the last dropped marble
% Arguments: AdjacentMarbles

get_opposite_direction(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn) :-
    OppositeRow is 2 * LastRow - Row,
    OppositeColumn is 2 * LastColumn - Column.

% Predicate to get the momentum direction
% Arguments: LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn

apply_momentum_to_adjacent_marbles([]).
apply_momentum_to_adjacent_marbles([(Row, Column) | Rest]) :-
    apply_momentum(Row, Column),
    apply_momentum_to_adjacent_marbles(Rest).

% Predicate to apply the momentum to the adjacent marbles
% Arguments: AdjacentMarbles

apply_momentum(Row, Column) :-
    last_dropped_marble(LastRow, LastColumn),
    get_opposite_direction(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn),
    apply_momentum_to_directions(Row, Column, OppositeRow, OppositeColumn). % Incompleto

% Predicate to apply the momentum
% Arguments: Player, Row, Column


apply_momentum_to_directions(Row, Column, OppositeRow, OppositeColumn) :-
     can_move_marble( OppositeRow, OppositeColumn),
     transfer(Row, Column, OppositeRow, OppositeColumn). 


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

get_opposite_marbles_recursive :-
    last_dropped_marble(LastRow, LastColumn),
    adjacent_marbles(InitialAdjacentMarbles),
    get_opposite_marbles(LastRow, LastColumn, InitialAdjacentMarbles, UpdatedAdjacentMarbles),
    retractall(adjacent_marbles(_,_)),
    asserta(adjacent_marbles(UpdatedAdjacentMarbles)).

% Predicate to get the opposite marbles to the last dropped marble

get_opposite_marbles(_, _, AdjacentMarbles, AdjacentMarbles) :-
    \+ update_adjacent_marbles(_, _, AdjacentMarbles, _).

get_opposite_marbles(LastRow, LastColumn, AdjacentMarbles, FinalAdjacentMarbles) :-
    update_adjacent_marbles(LastRow, LastColumn, AdjacentMarbles, UpdatedAdjacentMarbles),
    get_opposite_marbles(LastRow, LastColumn, UpdatedAdjacentMarbles, FinalAdjacentMarbles).

% Predicate to get the opposite marbles to the last dropped marble
% Arguments: LastRow, LastColumn, AdjacentMarbles, FinalAdjacentMarbles

update_adjacent_marbles(LastRow, LastColumn, AdjacentMarbles, NewAdjacentMarbles,MarblesOnBoard) :-
    marbles_on_board(MarblesOnBoard),
    findall(_, (
        member((Row, Column), AdjacentMarbles),
        get_opposite_direction(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn),
        is_marble_at(OppositeRow, OppositeColumn,MarblesOnBoard),
        delete(AdjacentMarbles, (Row, Column), UpdatedAdjacentMarbles),
        append(UpdatedAdjacentMarbles, [(OppositeRow, OppositeColumn)], NewAdjacentMarbles)
    ), _).

% Predicate to update the adjacent marbles
% Arguments: LastRow, LastColumn, AdjacentMarbles, NewAdjacentMarbles

