% Print a row with indices
print_row_with_indices([], _).
print_row_with_indices([X|Xs], Index) :-
    write(Index),          % Print the row index
    write(' '),            % Space after the index
    print_row(X),          % Print the row content
    nl,                   % Newline
    NewIndex is Index + 1, % Increment the index
    print_row_with_indices(Xs, NewIndex).

% Print a row
print_row([]) :- write('|'), nl.
print_row([X|Xs]) :-
    write('|'),          % Start cell with |
    write(X),            % Print the cell content
    write(' '),          % Space after the cell content
    print_row(Xs).       % Continue with the next cell

% Print column indices
print_column_indices(Current, Max) :-
    write('    '), % Space before the column indices
    print_column_indices(Current, Max, 1).

print_column_indices(Current, Max, _) :-
    Current > Max,
    nl. % Go to the next line when all indices are printed
print_column_indices(Current, Max, Col) :-
    write(Col),     % Print the column index
    write('  '),     % Space after the index
    NextCol is Col + 1,
    NextCurrent is Current + 1,
    print_column_indices(NextCurrent, Max, NextCol).

% Print rows
print_rows([], _).
print_rows([Row|Rest], Index) :-
    print_row_with_indices(Row, Index), % Print the row with index
    NextIndex is Index + 1,
    print_rows(Rest, NextIndex).       % Continue with the next row

% To print the board, call the following predicate:
print_board([Board,_,_,_]) :-
    print_column_indices(1, 7), % Print column indices
    print_rows(Board, 1).       % Print rows with row indices

% Update the board with marbles from MarblesOnBoard and then print the updated board
update_board([Board, _, MarblesOnBoard, _]) :-
    update_board_cells(Board, MarblesOnBoard, UpdatedBoard),
    print_board([UpdatedBoard, _, _, _]).

% Helper predicate to update the board cells with marbles
update_board_cells(Board, MarblesOnBoard, UpdatedBoard) :-
    update_board_cells(Board, MarblesOnBoard, 1, UpdatedBoard).

update_board_cells([], _, _, []).
% Update the current row and continue with the next row
update_board_cells([Row | Rest], MarblesOnBoard, Index, [UpdatedRow | UpdatedRest]) :-
    update_row(Row, MarblesOnBoard, Index, UpdatedRow),
    NewIndex is Index + 1,
    update_board_cells(Rest, MarblesOnBoard, NewIndex, UpdatedRest).

% Helper predicate to update a row with marbles
update_row(Row, MarblesOnBoard, Index, UpdatedRow) :-
    update_row_cells(Row, MarblesOnBoard, Index, 1, UpdatedRow).

update_row_cells([], _, _, _, []).
update_row_cells([empty | Rest], MarblesOnBoard, Index, Col, [empty | UpdatedRest]) :-
    !, % Skip empty cells, no need to check MarblesOnBoard
    NewCol is Col + 1,
    update_row_cells(Rest, MarblesOnBoard, Index, NewCol, UpdatedRest).
update_row_cells([empty | Rest], MarblesOnBoard, Index, Col, [Player | UpdatedRest]) :-
    (   member((Player, Index, Col), MarblesOnBoard) ->
        true
    ;   Player = empty 
    ),
    NewCol is Col + 1,
    update_row_cells(Rest, MarblesOnBoard, Index, NewCol, UpdatedRest).
update_row_cells([Cell | Rest], MarblesOnBoard, Index, Col, [Cell | UpdatedRest]) :-
    NewCol is Col + 1,
    update_row_cells(Rest, MarblesOnBoard, Index, NewCol, UpdatedRest).


