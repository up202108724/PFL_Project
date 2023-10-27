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

% Print the entire board
print_board(Board) :-
    print_column_indices(1, 7), % Print column indices
    print_rows(Board, 1).       % Print rows with row indices

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
print_board([Board,_,_]) :-
    print_column_indices(1, 7), % Print column indices
    print_rows(Board, 1).       % Print rows with row indices