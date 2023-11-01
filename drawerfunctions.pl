board([
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty]
]).

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

% Define an example of MarblesOnBoard.
marbles_on_board([(player1, 2, 3), (player2, 3, 4)]).

% Main predicate to update the board and print it.
main :-
    board(Board), % Get the initial board.
    marbles_on_board(Marbles), % Get the list of coordinates.
    update_board(Marbles, Board, UpdatedBoard, _), % Update the board.
    print_board(UpdatedBoard). % Print the updated board.

% Define a predicate to print the board.
print_board([]).
print_board([Row | Rest]) :-
    write(Row), nl,
    print_board(Rest).