:- use_module(library(lists)).
:- consult(dynamicfunctions).
:- consult(board).
:- consult(drawerfunctions).
:- consult(configurations).
% option(+N)
% Game mode options.

% game_configurations(-GameState) 
% Set the game configurations 
game_configurations([Board,Player,[],0]):-     
       header,
       set_mode,
       init_empty_board(7,Board),
       initialize_marbles([]),
       choose_player(Player).
    

% game_cycle(-GameState)
% Recursive loop while the game is not over

game_cycle(GameState):-
    [_, Player,MarblesOnBoard, _] = GameState,
    is_terminal_state(Player,MarblesOnBoard),!,
    clear_data.
game_cycle(GameState):-
    move(GameState, NewGameState),
    [Board, Player, MarblesOnBoard, TotalMoves] = NewGameState,
    change_player(Player, NewPlayer),
    print_board(Board),
    format('Started player ~w~n', [NewPlayer]),
    game_cycle([Board, NewPlayer, MarblesOnBoard, TotalMoves]).

% move(GameState, NewGameState)
% Game action that builds a new GameState, representing a new move on the game 

move(GameState, NewGameState) :-
    [Board, Player,_, TotalMoves] = GameState,
     (is_bot(Player) ->  
        % Implement bot logic
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



    





