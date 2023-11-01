:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(dynamicfunctions).
:- consult(board).
:- consult(drawerfunctions).
:- consult(configurations).
:- consult(botutils).
% option(+N)
% Game mode options.

:-dynamic board_size/1.

% game_configurations(-GameState) 
% Set the game configurations 
game_configurations([Board,Player,[],0]):-     
       header,
       set_mode,
       choose_board_size(7, Size),
       initialize_board(Size,Board),
       initialize_marbles([]),
       choose_player(Player),
       assertz(board_size(Size)).

% game_cycle(-GameState)
% Recursive loop while the game is not over

game_cycle(GameState):-
    [_, Player,MarblesOnBoard, _] = GameState,
    is_terminal_state(Player,MarblesOnBoard),!,
    clear_data.
game_cycle(GameState):-
    board_size(Size),
    move(GameState, NewGameState),
    [_, Player, MarblesOnBoard, TotalMoves] = NewGameState,
    change_player(Player, NewPlayer),
    format('Started player ~w~n', [NewPlayer]),
    initialize_board(Size,X),
    fill_board(X,MarblesOnBoard,NewBoard),
    print_board(NewBoard),
    game_cycle([NewBoard, NewPlayer, MarblesOnBoard, TotalMoves]).

% move(GameState, NewGameState)
% Game action that builds a new GameState, representing a new move on the game 

move(GameState, NewGameState) :-
    [Board, Player,_, TotalMoves] = GameState,
    board_size(Size), 
     (is_bot(Player) ->  
        % Implement bot logic
        generate_all_coordinates(Size,Coordinates),
        filter_available_moves(Coordinates, MarblesOnBoard, AvailableMoves),
        random_member((Row,Column), AvailableMoves),
        place_marble(Player, Row, Column),
        NewTotalMoves is TotalMoves + 1
    ;   
        choose_position(Player),
        NewTotalMoves is TotalMoves + 1
    ),
    write('Total Moves: '), write(NewTotalMoves), nl,
    marbles_on_board(X),
    NewGameState = [Board,Player,X, NewTotalMoves].

% play
% Starts the game and clears data when it ends

play:-
    init_random_state,
    game_configurations(GameState),!,
    game_cycle(GameState).
    

% choose_position(+Player)
% Choose a position to place a marble

choose_position(Player):-
    write('Enter the row (1-7): '),
    read_number(Row),
    write('Enter the column (1-7): '),
    read_number(Column),
    (place_marble(Player, Row, Column) ->
        true;   write('Invalid position. Please choose a valid position.\n'),
        choose_position(Player)
    ).



    





