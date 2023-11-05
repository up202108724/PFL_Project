:- use_module(library(lists)).
:- consult(configurations).
:- dynamic last_dropped_marble/2.

:-dynamic marbles_on_board/1.
:-dynamic adjacent_marbles/2.

initialize_marbles(MarblesonBoard) :-
    assertz(marbles_on_board(MarblesonBoard)).

% Predicate to initialize the marbles on the board

init_dynamic_adjacent_marbles(MarblesonBoard) :-
    assertz(adjacent_marbles([], MarblesonBoard)).

% Predicate to initialize the adjacent marbles

print_list([]).
print_list([Head|Tail]) :-
    write(Head),   % Print the current element
    nl,            % Add a newline for readability
    print_list(Tail).  % Print the rest of the list

% Predicate to print a list

initial_state(N, Board) :- initialize_board(N, N, Board).

% Define the size of the square board.

initialize_board(0, _, []).
initialize_board(N, M, [Row | Rest]) :- 
    N > 0,
    length(Row, M),
    N1 is N - 1,
    initialize_row(M, Row),
    initialize_board(N1, M, Rest).

% Define a predicate to initialize the board.

initialize_row(0, []).
initialize_row(N, [empty | Rest]) :- 
    N > 0,
    N1 is N - 1,
    initialize_row(N1, Rest).

% Define a predicate to initialize a row.

within_boundaries(Row,Column):-
    board_size(Size),
    Row >=1 ,
    Row =<Size,
    Column >=1,
    Column =<Size.

% Predicate to check if a marble is within the board boundaries
% Arguments: Row, Column


place_marble(Player, Row, Column):-
    marbles_on_board(MarblesOnBoard),
    within_boundaries(Row, Column),
    print_list(MarblesOnBoard),
    \+ is_marble_at(_, Row, Column,MarblesOnBoard),
    format('Put on empty space~n', []),
    NewMarble = (Player, Row, Column),
    append(MarblesOnBoard, [NewMarble], UpdatedMarblesOnBoard),
    retractall(marbles_on_board(_)), % Remove the old state
    assertz(marbles_on_board(UpdatedMarblesOnBoard)),
    write("UpdatedMarblesOnBoard: "), write(UpdatedMarblesOnBoard), nl,
    format('Updated pieces on board!~n',[]),
    set_last_dropped_marble(Row, Column),
    retractall(adjacent_marbles(_)),
    adjacent_marbles(AdjacentMarbles,UpdatedMarblesOnBoard),
    write("AdjacentMarbles: "), write(AdjacentMarbles), nl,
    get_updated_adjacent_marbles(AdjacentMarbles,UpdatedAdjacentMarbles),
    write("AdjacentMarbles: "), write(UpdatedAdjacentMarbles), nl,
    apply_momentum_to_marbles(UpdatedAdjacentMarbles,UpdatedMarblesOnBoard).

% Predicate to place a marble on the board
% Arguments: Player, Row, Column

replace_marble(Player, Row, Column):-
        retract(marbles_on_board(_)),
        NewMarble = (Player, Row, Column),
        append([], [NewMarble], UpdatedMarblesOnBoard),
        assertz(marbles_on_board(UpdatedMarblesOnBoard)),
        write("works"), nl.



is_marble_at(Player, Row, Column, MarblesOnBoard) :-
    member((P, R, C), MarblesOnBoard),
    Player = P,
    Row = R,
    Column = C.


% Predicate to check if a marble is at a certain position
% Arguments: Row, Column

transfer(Row, Column, NewRow, NewColumn,NewMarblesOnBoard) :- % Temporario 
    within_boundaries(NewRow, NewColumn),
    marbles_on_board(MarblesOnBoard),
    is_marble_at(Player,Row, Column,MarblesOnBoard),
    \+ is_marble_at(_,NewRow, NewColumn,MarblesOnBoard),
    delete(MarblesOnBoard, (Player, Row, Column), TempMarbles),
    retract(marbles_on_board(_)),
    NewMarblesOnBoard = [(Player, NewRow, NewColumn) | TempMarbles],
    assertz(marbles_on_board(NewMarblesOnBoard)),
    format("transfer working!~n",[]).

% Predicate to transfer a marble from one position to another 
% Arguments: Player, Row, Column, Direction 
    
can_move_marble( NewRow, NewColumn, MarblesOnBoard) :-
    within_boundaries(NewRow, NewColumn),
    \+ has_adjacent_marble(NewRow, NewColumn,MarblesOnBoard).

% Predicate to check if a marble can be moved to a certain position
% Arguments: NewRow, NewColumn, MarblesOnBoard

has_adjacent_marble(Row, Column,MarblesOnBoard) :-
    is_marble_at(_,Row, Column,MarblesOnBoard).

% Predicate to check if a marble has an adjacent marble
% Arguments: Row, Column, MarblesOnBoard


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

adjacent_marbles(AdjacentMarbles,MarblesOnBoard) :-
    last_dropped_marble(LastRow, LastColumn),
    findall((NewRow, NewColumn), 
            (adjacent_position(LastRow, LastColumn, NewRow, NewColumn), 
             is_marble_at(_,NewRow, NewColumn,MarblesOnBoard)),
            AdjacentMarbles).

% Predicate to get the adjacent marbles of the last dropped marble
% Arguments: AdjacentMarbles

apply_momentum_to_marbles([],_).
apply_momentum_to_marbles([(Row, Column) | Rest],MarblesOnBoard) :-
    format("Applying momentum~n", []),
    write("Row: "), write(Row), nl,
    write("Column: "), write(Column), nl,
    apply_momentum(Row, Column,MarblesOnBoard),
    apply_momentum_to_marbles(Rest,MarblesOnBoard).

% Predicate to apply the momentum to the adjacent marbles
% Arguments: AdjacentMarbles

apply_momentum(Row, Column,MarblesOnBoard) :-
    last_dropped_marble(LastRow, LastColumn),
    format("Reached apply_momentum~n", []),
    get_next_marble(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn),
    write("OppositeRow: "), write(OppositeRow), nl,
    write("OppositeColumn: "), write(OppositeColumn), nl,
    apply_momentum_to_directions(Row, Column, OppositeRow, OppositeColumn,NewMarblesOnBoard). 
    
% Predicate to apply the momentum
% Arguments: Player, Row, Column


apply_momentum_to_directions(Row, Column, OppositeRow, OppositeColumn,NewMarblesOnBoard) :-
    
    (within_boundaries(OppositeRow, OppositeColumn) ->
        true,
        transfer(Row, Column,OppositeRow, OppositeColumn,NewMarblesOnBoard)
        
        ;   format("Invalid position.~n", []),
        marbles_on_board(NewMarblesOnBoard),
        is_marble_at(Player,Row, Column,NewMarblesOnBoard),
        marbles_on_board(NewMarblesOnBoard),
        delete(NewMarblesOnBoard, (Player, Row, Column), TempMarbles),
        retract(marbles_on_board(_)),
        assertz(marbles_on_board(TempMarbles))
        % There is a problem
    ).


% Predicate to apply the momentum to the directions
% Arguments: Player, Row, Column

set_last_dropped_marble(Row, Column) :-
    retractall(last_dropped_marble(_, _)), 
    asserta(last_dropped_marble(Row, Column)).

% Predicate to set the last dropped marble
% Arguments: Row, Column

last_dropped_marble(Row, Column) :-
    last_dropped_marble(Row, Column).

% Predicate to get the last dropped marble
% Arguments: Row, Column

get_updated_adjacent_marbles(AdjacentMarbles,UpdatedAdjacentMarbles) :-
    marbles_on_board(MarblesOnBoard),
    last_dropped_marble(LastRow, LastColumn),
    write("InitialAdjacentMarbles: "), write(InitialAdjacentMarbles), nl,
    get_adjacent_marbles_recursive(LastRow, LastColumn, AdjacentMarbles, UpdatedAdjacentMarbles),
    write("UpdatedAdjacentMarbles: "), write(UpdatedAdjacentMarbles), nl,
    (UpdatedAdjacentMarbles = [] ->
        true; % Do nothing if UpdatedAdjacentMarbles is empty
        (retractall(adjacent_marbles(_)),
        assertz(adjacent_marbles(UpdatedAdjacentMarbles, MarblesOnBoard)))
    ).

% Predicate to get the opposite marbles to the last dropped marble
% Arguments: UpdatedAdjacentMarbles

get_adjacent_marbles_recursive(_, _, [], _).

get_adjacent_marbles_recursive(LastRow, LastColumn, [(Row, Column) | RestAdjacentMarbles], FinalAdjacentMarbles) :-
    format("AdjacentMarbles: ~n", []),
    write((Row, Column)), nl,
    marbles_on_board(MarblesOnBoard),
    update_adjacent_marbles(LastRow, LastColumn, [(Row, Column) | RestAdjacentMarbles], UpdatedAdjacentMarbles,MarblesOnBoard),
    format("UpdatedAdjacentMarbles: ~n", []),
    write(UpdatedAdjacentMarbles), nl,
    get_adjacent_marbles_recursive(LastRow, LastColumn, RestAdjacentMarbles, RestFinalAdjacentMarbles),
    format("RestFinalAdjacentMarbles: ~n", []), 
    write(RestFinalAdjacentMarbles), nl,
    append(UpdatedAdjacentMarbles, RestFinalAdjacentMarbles, FinalAdjacentMarbles).

% Predicate to get the adjacent marbles to the last dropped marble
% Arguments: LastRow, LastColumn, AdjacentMarbles, FinalAdjacentMarbles

update_adjacent_marbles(_, _, [], _, _).

update_adjacent_marbles(LastRow, LastColumn, [(Row, Column) | _ ], NewAdjacentMarbles, MarblesOnBoard) :-
    write("AdjacentMarbles: "), write([(Row, Column)]), nl,
    write("Row: "), write(Row), nl,
    write("Column: "), write(Column), nl,
    write("LastRow: "), write(LastRow), nl,
    write("LastColumn: "), write(LastColumn), nl,
    get_next_marble(LastRow, LastColumn, Row, Column, OppositeRow, OppositeColumn),
    write("OppositeRow: "), write(OppositeRow), nl,
    write("OppositeColumn: "), write(OppositeColumn), nl,
    (is_marble_at(_, OppositeRow, OppositeColumn, MarblesOnBoard) ->
        format("Its marble at~n", []),
            update_adjacent_marbles(LastRow, LastColumn, [(OppositeRow, OppositeColumn)], NewAdjacentMarbles, MarblesOnBoard)
        ;
        format("Its not marble at~n", []),
            NewAdjacentMarbles = [(Row, Column)],
            write("NewAdjacentMarbles: "),
            print_list(NewAdjacentMarbles), nl
     
    ).
% Predicate to update the adjacent marbles
% Arguments: LastRow, LastColumn, AdjacentMarbles, NewAdjacentMarbles

calculate_direction(LastRow, LastColumn, Row, Column, (DirectionX, DirectionY)) :-
    DisX is Row - LastRow,
    DisY is Column - LastColumn,
    format("Distance in X (~d)", [DisX] ),
    format("Distance in Y (~d)", [DisY] ),
    normalize_direction((DisX, DisY), (DirectionX, DirectionY)).

% Predicate to calculate the direction of the momentum
% Arguments: LastRow, LastColumn, Row, Column, Direction


normalize_direction((0, 0), (0, 0)).
normalize_direction((0, Y), (0, 1)) :- Y > 0.
normalize_direction((0, Y), (0, -1)) :- Y < 0.
normalize_direction((X, 0), (1, 0)) :- X > 0.
normalize_direction((X, 0), (-1, 0)) :- X < 0.
normalize_direction((X,Y), (1,1)):- X > 0, Y > 0.
normalize_direction((X,Y), (-1,-1)):- X < 0, Y < 0.
normalize_direction((X,Y), (1,-1)):- X > 0, Y < 0.
normalize_direction((X,Y), (-1,1)):- X < 0, Y > 0. 

% Predicate to normalize the direction of the momentum
% Arguments: DisX, DisY, Direction

get_next_marble(LastRow, LastColumn, Row, Column, NextRow, NextColumn) :-
    format("Reaches here: ~n", []),
    format("Last Row (~d)", [LastRow] ),
    format("Last ColumnY (~d)", [LastColumn] ),
    format("Row  (~d)", [Row] ),
    format("Column  (~d)", [Column] ),
    calculate_direction(LastRow, LastColumn, Row, Column, (DirectionX, DirectionY)),
    format("DirectionX (~d)", [DirectionX] ),
    format("DirectionY (~d)", [DirectionY] ),
    NextRow is Row + DirectionX,
    NextColumn is Column + DirectionY,
    format("Next Row (~d)", [NextRow] ),
    format("Next Column (~d)", [NextColumn] ).

% Predicate to get the next marble adjacent to the marble in row and column in relation to the last dropped marble
% Arguments: LastRow, LastColumn, Row, Column, NextRow, NextColumn, MarblesOnBoard
