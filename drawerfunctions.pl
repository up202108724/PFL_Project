:- use_module(library(lists)).

% Fill the board with marbles
fill_board(Board, [], Board).
fill_board(Board, [(Player, X, Y) | RestMarbles], NewBoard) :-
    nth1(X, Board, Row),               % Get the X-th row
    nth1(Y, Row, empty),               % Check if the cell is empty
    replace_element(Board, X, Y, Player, UpdatedBoard), % Place the marble
    fill_board(UpdatedBoard, RestMarbles, NewBoard).

% Replace an element at a specific row and column
replace_element(Board, X, Y, NewValue, NewBoard) :-
    nth1(X, Board, Row),             % Get the X-th row
    replace_nth(Y, Row, NewValue, UpdatedRow), % Replace the Y-th element in the row
    replace_nth(X, Board, UpdatedRow, NewBoard).

% Replace the N-th element in a list
replace_nth(1, [_ | Rest], NewValue, [NewValue | Rest]).
replace_nth(N, [X | Rest], NewValue, [X | UpdatedRest]) :-
    N > 1,
    N1 is N - 1,
    replace_nth(N1, Rest, NewValue, UpdatedRest).


% Define a predicate to print the board.
print_board([]).
print_board([Row | Rest]) :-
    print_row(Row),
    nl,
    print_board(Rest).

% Define a predicate to print a row.
print_row([]).
print_row([X | Rest]) :-
    write(X),        % Print the element X
    write(' '),       % Add a space between elements
    print_row(Rest).

    
