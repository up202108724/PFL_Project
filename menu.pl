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
    [Board, _,MarblesOnBoard,TotalMoves] = NewGameState,
    update_board([Board,_,MarblesOnBoard,_]),
    change_player(Player,NewPlayer).
    NewGameState = [Board, NewPlayer,MarblesOnBoard, TotalMoves].
    game_cycle(NewGameState).
    

% move(GameState, NewGameState)
% Game action that builds a new GameState, representing a new move on the game 

move(GameState, NewGameState) :-
    [Board, Player,MarblesOnBoard, TotalMoves] = GameState,
     (is_bot(Player) ->  
        % Implement bot logic
        NewTotalMoves is TotalMoves + 1
    ;   
        choose_position(Player),
        NewTotalMoves is TotalMoves + 1
    ),
    marbles_on_board(MarblesOnBoard),
    NewGameState = [Board,_,MarblesOnBoard, NewTotalMoves].

% play
% Starts the game and clears data when it ends

play:-
    game_configurations(GameState),!,
    game_cycle(GameState).

% choose_position(+Player)
% Choose a position to place a marble

choose_position(Player):-
    write('Enter the row (1-7) and column (1-7) to place a marble (e.g., "3-4"): '),
    read(Position),
    (valid_position(Position, Player) ->
        true
    ;   format('Invalid position. Please choose a valid position.\n'),
        choose_position(Player)
    ).

% valid_position(+Position, +Player)
% Checks if the position is valid

valid_position(Position, Player):-
    atom_string(Position, PositionStr),
    split_string(PositionStr, "-", "", [RowStr, ColStr]),
    number_string(Row, RowStr),
    number_string(Col, ColStr),
    place_marble(Player, Row, Col).



    





