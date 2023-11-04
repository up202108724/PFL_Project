:- use_module(library(lists)).

board([
    [empty, player1, empty],
    [empty, player2, player1],
    [player1, empty, player2]
]).

% Fill the board with marbles
update_board_with_new_coordinates(Board, MarblesOnBoard, NewBoard) :-
    % Iterate through MarblesOnBoard and update the corresponding cells in the Board
    update_board_cells1(Board, MarblesOnBoard, NewBoard).

erasing_old_coordinates(Board, MarblesOnBoard, NewBoard) :-
    % Iterate through MarblesOnBoard and update the corresponding cells in the Board
    update_board_cells2(Board, MarblesOnBoard, NewBoard).
% Base case: All coordinates are processed, no more replacements needed
update_board_cells1(Board, [], Board).

% Recursive case: Replace the corresponding cell and continue with the rest of the coordinates
update_board_cells1(Board, [(Player, X, Y) | RestCoords], NewBoard) :-
    nth1(X, Board, Row),          % Get the X-th row
    nth1(Y, Row, empty),          % Check if the cell is empty
    update_board(Board, Player , X , Y, UpdatedBoard), % Place the marble
    update_board_cells1(UpdatedBoard, RestCoords, NewBoard).

update_board_cells2(Board, [], Board).

% Handle the case when a coordinate from the previous state is no longer in MarblesOnBoard
update_board_cells2(Board, [(Player, X, Y) | RestCoords], NewBoard) :-
    nth1(X, Board, Row),          % Get the X-th row
    nth1(Y, Row, Player),         % Check if the cell is occupied by a player
    update_board(Board, empty,X, Y, UpdatedBoard), % Replace with an empty cell
    update_board_cells2(UpdatedBoard, RestCoords, NewBoard).

% Define a predicate to print the board.

update_board(Board, Content, X, Y, NewBoard) :-
    update_board_cell(Board, Content, X, Y, NewBoard).

% update_board_cell(Board, Content, X, Y, NewBoard)
% Helper predicate to update a single cell in the 2D array.
update_board_cell(Board, Content, X, Y, NewBoard) :-
    update_row(Board, Content, X, Y, NewBoard).

% update_row(Board, Content, X, Y, NewBoard)
% Helper predicate to update a single cell in a row.
update_row([Row|Rest], Content, 1, Y, [UpdatedRow|Rest]) :-
    update_column(Row, Content, Y, UpdatedRow).

update_row([Row|Rest], Content, X, Y, [Row|UpdatedRest]) :-
    X > 1,
    X1 is X - 1,
    update_row(Rest, Content, X1, Y, UpdatedRest).

% update_column(Row, Content, Y, UpdatedRow)
% Helper predicate to update a single cell in a row.
update_column([_|Rest], Content, 1, [Content|Rest]).

update_column([Element|Rest], Content, Y, [Element|UpdatedRest]) :-
    Y > 1,
    Y1 is Y - 1,
    update_column(Rest, Content, Y1, UpdatedRest).

% Define a predicate to print the board
print_board(Board) :-
    print_board_rows(Board),
    print_horizontal_border(Board).

% Define a predicate to print the horizontal border based on the size of the board
print_horizontal_border([]).
print_horizontal_border([_|Rest]) :-
    write('+---'),
    print_horizontal_border(Rest).
print_horizontal_border(Board) :-
    length(Board, N),
    print_horizontal_border(N).

% Define a predicate to print the rows of the board
print_board_rows([]).
print_board_rows([Row | Rest]) :-
    print_horizontal_border(Row),
    nl,
    print_row(Row),
    nl,
    print_board_rows(Rest).

% Define a predicate to print a row
print_row([]).
print_row([X | Rest]) :-
    print_cell(X),
    print_row(Rest).

% Define a predicate to print a cell (element) of the row
print_cell(player1) :- write('| P1').
print_cell(player2) :- write('| P2').
print_cell(empty)   :- write('|   ').


clear_console:-
    write('\33\[2J').    
