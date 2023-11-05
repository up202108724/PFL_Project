:-use_module(library(between)).
:-use_module(library(lists)).

% generate_all_coordinates(+Size, -Coordinates)
% Generate all the coordinates of the board
generate_all_coordinates(Size,Coordinates):-
    findall((X,Y),(between(1,Size,X),between(1,Size,Y)),Coordinates).

% coord_taken(+MarblesOnBoard, +Coordinate)
% Check if a coordinate is taken by a marble
coord_taken(MarblesOnBoard,(X,Y)):-
    member((_,X,Y), MarblesOnBoard).

% filter_available_moves(+Coordinates, +MarblesOnBoard, -AvailableMoves)
% Filter the coordinates that are not taken by a marble
filter_available_moves(Coordinates, MarblesOnBoard, AvailableMoves):-
    exclude(coord_taken(MarblesOnBoard), Coordinates, AvailableMoves).

sample_marbles([(Player1,2,3), (Player2,3,4),(Player1,2,3), (Player2,3,4),(Player1,2,3), (Player2,3,4)]).          

init_random_state:-
    now(X),
    setrand(X).