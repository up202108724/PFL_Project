:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system), [now/1]).
:- consult(dynamicfunctions).
:- consult(board).
:- consult(drawerfunctions).
:- consult(configurations).
:- consult(botutils).


:-dynamic board_size/1.
:-dynamic winner/1.
:-dynamic actual_marbles_on_board/1.
:-dynamic simulated_marbles_on_board/1.
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
    winner(Winner),
    name_of(Winner,NameofWinner),
    format('The game is over! The winner is ~w~n', [NameofWinner]).
game_cycle(GameState):-
    [Board, Player, MarblesOnBoard, TotalMoves] = GameState,
    erasing_old_coordinates(Board, MarblesOnBoard, ErasedBoard),
    name_of(Player, NameofPlayer),
    format('Started player ~w~n', [NameofPlayer]),
    move(GameState, NewGameState),
    marbles_on_board(MarblesOnBoard2),
    update_board_with_new_coordinates(ErasedBoard, MarblesOnBoard2, NewBoard),
    change_player(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState2 =[NewBoard, NewPlayer, MarblesOnBoard2, NewTotalMoves],
    display_state(NewGameState2),
    game_cycle([NewBoard, NewPlayer, MarblesOnBoard2, NewTotalMoves]).

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
        place_marble(Player, Row, Column)
    ;   
        choose_position(Player)
    ),
    
    marbles_on_board(X),
    NewGameState = [Board,Player,X, NewTotalMoves].

% choose_position(+Player)
% Choose a position to place a marble

choose_position(Player):-
    board_size(Size),
    format('Enter the row (1-~d)', [Size]),
    read_number(Row),
    format('Enter the column (1-~d)', [Size]),
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
    NumMarbles is 100.

% Predicate to check if a player has won the game
% Arguments: Player, Board
display_state(GameState):-
    clear_console,
    [Board,_,_,TotalMoves]= GameState,
    print_board(Board), nl, 
    format('Total Moves:~d)', [TotalMoves]), nl.

game_over(GameState, Winner) :-
    [_,_,MarblesOnBoard,_]= GameState,
    change_player(Winner, Opponent),
    has_won_game(Opponent, MarblesOnBoard),!,
    assertz(winner(Opponent)).
game_over(GameState,Winner):-
    [_,Player,MarblesOnBoard,TotalMoves]=GameState,
    TotalMoves is 60,
    change_player(Player,OtherPlayer),
    findall((Player, _, _), member((Player, _, _), MarblesOnBoard), Player1Marbles),
    findall((OtherPlayer, _, _), member((NewPlayer, _, _), MarblesOnBoard), Player2Marbles),
    length(Player1Marbles, NumMarbles1),
    length(Player2Marbles, NumMarbles2),
    (NumMarbles1 > NumMarbles2 -> true , assertz(winner(Player)); NumMarbles2 > NumMarbles1 -> true, assertz(winner(OtherPlayer)); assertz(winner('Draw'))).

clear_data:-
    retractall(board_size(_)),
    retractall(name_of(_,_)),
    retractall(last_dropped_marble(_,_)),
    retractall(marbles_on_board(_)),
    retractall(winner(_)),
    retractall(adjacent_marbles(_)).

% play
% Starts the game and clears data when it ends

play:-
    game_configurations(GameState),!,
    game_cycle(GameState),
    format('Clearing data ~n', []),
    clear_data,
    format('Cleared data ~n', []).
    


