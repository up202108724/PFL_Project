:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system), [now/1]).
:- consult(dynamicfunctions).
:- consult(board).
:- consult(drawerfunctions).
:- consult(configurations).
:- consult(botutils).


:-dynamic board_size/1.

% game_configurations(-GameState) 
% Set the game configurations 
game_configurations([Board,Player,[],0]):-     
       header,
       set_mode,
       choose_board_size(7, Size),
       initial_state(Size,Board),
       initialize_marbles([]),
       choose_player(Player),
       init_random_state,
       assertz(board_size(Size)).

% game_cycle(-GameState)
% Recursive loop while the game is not over

game_cycle(GameState):-
    [_, Player,_, _] = GameState,
    game_over(GameState,Player),!,
    fail.
game_cycle(GameState):-
    [Board, Player, MarblesOnBoard, TotalMoves] = GameState,
    erasing_old_coordinates(Board, MarblesOnBoard, ErasedBoard),
    format('erasing ~n', []),
    move(GameState, NewGameState),
    marbles_on_board(MarblesOnBoard2),
    format('she moved ~n', []),
    update_board_with_new_coordinates(ErasedBoard, MarblesOnBoard2, NewBoard),
    format('Updating new coordinates ~n', []),
    change_player(Player, NewPlayer),
    format('Started player ~w~n', [NewPlayer]),
    NewGameState2 =[NewBoard, NewPlayer, MarblesOnBoard2, TotalMoves],
    format('Showing the board ~w~n', [NewPlayer]),
    display_state(NewGameState2),
    format('Displaying yeah! ~w~n', [NewPlayer]),
    game_cycle([NewBoard, NewPlayer, MarblesOnBoard2, TotalMoves]).

% move(GameState, NewGameState)
% Game action that builds a new GameState, representing a new move on the game 

move(GameState, NewGameState) :-
    [Board, Player,MarblesOnBoard, TotalMoves] = GameState,
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

% choose_position(+Player)
% Choose a position to place a marble

choose_position(Player):-
    write('Enter the row (1-7): '),
    read_number(Row),
    write('Enter the column (1-7): '),
    read_number(Column),
    (place_marble(Player, Row, Column) ->
        true;   
        write('Invalid position. Please choose a valid position.\n'),
        choose_position(Player)
    ).

% clear_data    
% Clears all the data from the game
has_won_game(Player, MarblesOnBoard) :-
    findall((Player, _, _), member((Player, _, _), MarblesOnBoard), PlayerMarbles),
    length(PlayerMarbles, NumMarbles),
    NumMarbles >= 3.

% Predicate to check if a player has won the game
% Arguments: Player, Board
display_state(GameState):-
    [Board,_,_,_]= GameState,
    print_board(Board).

game_over(GameState, Winner) :-
    [_,_,MarblesOnBoard,_]= GameState,
    change_player(Winner, Opponent),
    has_won_game(Opponent, MarblesOnBoard),
    !, % Cut here to stop backtracking
    Winner = Opponent,
    format('The Winner is ~n , congratulations!', [Winner]).

clear_data:-
    retractall(board_size(_)),
    retractall(name_of(_,_)),
    retractall(last_dropped_marble(_,_)),
    retractall(marbles_on_board(_)),
    retractall(adjacent_marbles(_,_)).

% play
% Starts the game and clears data when it ends

play:-
    game_configurations(GameState),!,
    game_cycle(GameState),
    clear_data.
    


