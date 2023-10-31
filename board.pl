:- use_module(library(lists)).
:- dynamic last_dropped_marble/2.

:-dynamic marbles_on_board/1.
:- dynamic adjacent_marbles/2.

initialize_marbles(MarblesonBoard) :-
    assertz(marbles_on_board(MarblesonBoard)).

init_dynamic_adjacent_marbles(MarblesonBoard) :-
    assertz(adjacent_marbles([], MarblesonBoard)).

board(7, [
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty]
        
]).
within_boundaries(Row,Column):-
    Row >=1 ,
    Row =<7,
    Column >=1,
    Column =<7.

% Predicate to check if a marble is within the board boundaries
% Arguments: Row, Column


% Predicate to place a marble on the board
% Arguments: Player, Row, Column

place_marble(Player, Row, Column):-
    marbles_on_board(MarblesOnBoard),
    within_boundaries(Row, Column),
    \+ is_marble_at(_, Row, Column,MarblesOnBoard),
    format('Put on empty space~n', []),
    NewMarble = (Player, Row, Column),
    append(MarblesOnBoard, [NewMarble], UpdatedMarblesOnBoard),
    retract(marbles_on_board(_)), % Remove the old state
    assertz(marbles_on_board(UpdatedMarblesOnBoard)),
    format('Updated pieces on board!~n',[]),
    set_last_dropped_marble(Row, Column),
    retractall(adjacent_marbles(_)),
    adjacent_marbles(AdjacentMarbles,MarblesOnBoard),
    get_opposite_marbles_recursive,
    apply_momentum_to_adjacent_marbles(AdjacentMarbles),
    format("place_marble working!~n",[]).


is_marble_at(Player, Row, Column, MarblesOnBoard) :-
    member((P, R, C), MarblesOnBoard),
    Player = P,
    Row = R,
    Column = C.


% Predicate to check if a marble is at a certain position
% Arguments: Row, Column

transfer(Row, Column, NewRow, NewColumn) :- % Temporario 
    within_boundaries(NewRow, NewColumn),
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
    within_boundaries(NewRow, NewColumn),
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
    last_dropped_marble(LastRow, LastColumn),
    write("LastRow: "), write(LastRow), nl,
    write("LastColumn: "), write(LastColumn), nl,
    findall((NewRow, NewColumn), 
            (adjacent_position(LastRow, LastColumn, NewRow, NewColumn), 
             is_marble_at(_,NewRow, NewColumn,MarblesOnBoard)),
            AdjacentMarbles).

% Predicate to get the adjacent marbles of the last dropped marble
% Arguments: AdjacentMarbles

get_opposite_direction(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn) :-
    % Check if LastRow and LastColumn are instantiated, or use defaults if not
    (   nonvar(LastRow), nonvar(LastColumn) ->
        true
    ;   LastRow = 0, LastColumn = 0 % Default values
    ),
    
    % Check if Row and Column are instantiated, or use defaults if not
    (   nonvar(Row), nonvar(Column) ->
        true
    ;   Row = 0, Column = 0 % Default values
    ),

    % Print the values of LastRow, LastColumn, Row, and Column

    % Calculate OppositeRow and OppositeColumn
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
    marbles_on_board(MarblesOnBoard),
    last_dropped_marble(LastRow, LastColumn),
    write("LastRow: "), write(LastRow), nl,
    write("LastColumn: "), write(LastColumn), nl,
    adjacent_marbles(InitialAdjacentMarbles,MarblesOnBoard),
    write("InitialAdjacentMarbles: "), write(InitialAdjacentMarbles), nl,
    get_opposite_marbles(LastRow, LastColumn, InitialAdjacentMarbles, UpdatedAdjacentMarbles),
    retractall(adjacent_marbles(_)),
    asserta(adjacent_marbles(UpdatedAdjacentMarbles,MarblesOnBoard)).

% Predicate to get the opposite marbles to the last dropped marble

get_opposite_marbles(_, _, [], []).
get_opposite_marbles(_, _, AdjacentMarbles, AdjacentMarbles) :-
    marbles_on_board(MarblesOnBoard),
    \+ update_adjacent_marbles(_, _, AdjacentMarbles, _,MarblesOnBoard).

get_opposite_marbles(LastRow, LastColumn, AdjacentMarbles, FinalAdjacentMarbles) :-
    marbles_on_board(MarblesOnBoard),
    update_adjacent_marbles(LastRow, LastColumn, AdjacentMarbles, UpdatedAdjacentMarbles,MarblesOnBoard),
    get_opposite_marbles(LastRow, LastColumn, UpdatedAdjacentMarbles, FinalAdjacentMarbles).

% Predicate to get the opposite marbles to the last dropped marble
% Arguments: LastRow, LastColumn, AdjacentMarbles, FinalAdjacentMarbles

update_adjacent_marbles(LastRow, LastColumn, AdjacentMarbles, NewAdjacentMarbles,MarblesOnBoard) :-
    marbles_on_board(MarblesOnBoard),
    findall(_, (
        member((Row, Column), AdjacentMarbles),
        get_opposite_direction(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn),
        is_marble_at(_,OppositeRow, OppositeColumn,MarblesOnBoard),
        delete(AdjacentMarbles, (Row, Column), UpdatedAdjacentMarbles),
        append(UpdatedAdjacentMarbles, [(OppositeRow, OppositeColumn)], NewAdjacentMarbles)
    ), _).

% Predicate to update the adjacent marbles
% Arguments: LastRow, LastColumn, AdjacentMarbles, NewAdjacentMarbles

