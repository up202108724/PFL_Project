% Define the predicate to update the board.
update_board([], Board, UpdatedBoard, UpdatedBoard).
update_board([(Player, X, Y) | Rest], Board, UpdatedBoard, FinalUpdatedBoard) :-
    % Update the board at position (X, Y) with the player.
    replace(Board, X, Y, Player, NewBoard),
    % Continue updating the rest of the coordinates.
    update_board(Rest, NewBoard, UpdatedBoard, FinalUpdatedBoard).

% Define a predicate to replace an element in a list.
replace([_|T], 0, Y, Value, [Value|T]).
replace([H|T], X, Y, Value, [H|R]) :-
    X > 0,
    X1 is X - 1,
    replace(T, X1, Y, Value, R).


% Define a predicate to print the board.
print_board([]).
print_board([Row | Rest]) :-
    print_row(Row), 
    nl,
    print_board(Rest).

print_row([]).
print_row([X | Rest]) :-
    write(X),        % Print the element X
    write(' '),       % Add a space between elements
    print_row(Rest).