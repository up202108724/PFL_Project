:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system), [now/1]).
:- consult(dynamicfunctions).
:- consult(board).
:- consult(drawerfunctions).
:- consult(configurations).
:- consult(botutils).

% board_size(-Size)
% Set the board size
:-dynamic board_size/1.

% winner(-Player)
% Set the winner of the game
:-dynamic winner/1.

% actual_marbles_on_board(+MarblesOnBoard)
% Set the temporary marbles on the board used in simulate_move
:-dynamic actual_marbles_on_board/1.


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
% Game cycle that ends when the game is over
game_cycle(GameState):-
    [_, Player,_, _] = GameState,
    game_over(GameState,Player),!,
    winner(Winner),
    name_of(Winner,NameofWinner),
    format('The game is over! The winner is ~w~n', [NameofWinner]).
game_cycle(GameState):-
    [Board,Player,X, NewTotalMoves]=GameState,
    name_of(Player, NameofPlayer),
    format('Started player ~w~n', [NameofPlayer]),
    move(GameState, NewGameState),
    game_cycle(NewGameState).

% move(GameState, NewGameState)
% Game action that builds a new GameState, representing a new move on the game 

move(GameState, NewGameState) :-
    [Board, Player, MarblesOnBoard, TotalMoves] = GameState,
    erasing_old_coordinates(Board, MarblesOnBoard, ErasedBoard),
    board_size(Size), 
    forced_moves(Player, Size, MarblesOnBoard, ForcedMoves),
    length(ForcedMoves, NumForcedMoves),
    (is_bot(Player) ->  
        (NumForcedMoves is 0 -> 
            generate_all_coordinates(Size, Coordinates),
            filter_available_moves(Coordinates, MarblesOnBoard, AvailableMoves),
            random_member((Row, Column), AvailableMoves),
            NewTotalMoves is TotalMoves + 1,
            place_marble(Player, Row, Column)
        ;
            random_member((Row, Column), ForcedMoves),
            place_marble(Player, Row, Column),
            NewTotalMoves is TotalMoves + 1
        )
    ;  
        (NumForcedMoves is 0 -> 
            choose_position(Player, TotalMoves),
            NewTotalMoves is TotalMoves + 1
        ;
        write('Forced Moves: '),
        print_list(ForcedMoves),
        choose_forced_position(Player,ForcedMoves),
        NewTotalMoves is TotalMoves + 1
        )
    ),
    change_player(Player, NewPlayer),
    marbles_on_board(X),
    update_board_with_new_coordinates(ErasedBoard, X, NewBoard),
    display_state([NewBoard, _, _, NewTotalMoves]),
    NewGameState = [NewBoard, NewPlayer, X, NewTotalMoves].

% choose_position(+Player)
% Choose a position to place a marble

choose_position(Player,TotalMoves):-
    board_size(Size),
    format('Enter the row (1-~d)', [Size]),
    read_number(Row),
    format('Enter the column (1-~d)', [Size]),
    read_number(Column),
    (place_marble(Player, Row, Column) ->
        true; 
        TotalMoves = 1 ->
        replace_marble(Player, Row, Column)
    ;
        write('Invalid position. Please choose a valid position.\n'),
        choose_position(Player,TotalMoves)
    ).

% choose_forced_position(+Player,+ForcedMoves)
% Choose one of the forced positions to place a marble
choose_forced_position(Player,ForcedMoves):-
    board_size(Size),
    write('Be careful, you are about to lose'),
    format('Enter the row (1-~d)', [Size]),
    read_number(Row),
    format('Enter the column (1-~d)', [Size]),
    read_number(Column),
    (member((Row,Column),ForcedMoves)->place_marble(Player, Row, Column); 
        write('Invalid position. Please choose a valid position.\n'),
        choose_forced_position(Player,ForcedMoves)
    ).    

% has_won_game(+Player, +MarblesOnBoard)
% Predicate to check if a player has won the game
has_won_game(Player, MarblesOnBoard) :-
    findall((Player, _, _), member((Player, _, _), MarblesOnBoard), PlayerMarbles),
    length(PlayerMarbles, NumMarbles),
    NumMarbles is 8.

% display_state(+GameState)
% Display the current state of the game
display_state(GameState):-
    clear_console,
    [Board,_,_,TotalMoves]= GameState,
    print_board(Board), nl, 
    format('Total Moves:~d', [TotalMoves]), nl.

% game_over(+GameState, -Winner)
% Predicate to check if the game is over
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

% simulate_move(+ActualMarblesOnBoard, +Move, -NewMarblesOnBoard)
% Simulate a move on the board
simulate_move(ActualMarblesOnBoard, (Player,X,Y), NewMarblesOnBoard):-
    assertz(actual_marbles_on_board(ActualMarblesOnBoard)),
    assertz(marbles_on_board(ActualMarblesOnBoard)),
    asserta(board_size(7)),
    (place_marble(Player,X,Y)->true; fail),
    marbles_on_board(NewMarblesOnBoard),
    retractall(marbles_on_board(_)),
    assertz(marbles_on_board(ActualMarblesOnBoard)),
    retractall(actual_marbles_on_board(_)).


% clear_data
% Clear all the data from the game when it ends
clear_data:-
    retractall(board_size(_)),
    retractall(name_of(_,_)),
    retractall(last_dropped_marble(_,_)),
    retractall(marbles_on_board(_)),
    retractall(winner(_)).

% play
% Starts the game
play:-
    game_configurations(GameState),!,
    game_cycle(GameState),
    format('Clearing data ~n', []),
    clear_data,
    format('Cleared data ~n', []).

% has_not_winning_anymore(+Player, +MarblesOnBoard)
% Predicate to check if the player is not winning the game
has_not_winning_anymore(Player, MarblesOnBoard) :-
    findall((Player, _, _), member((Player, _, _), MarblesOnBoard), PlayerMarbles),
    length(PlayerMarbles, NumMarbles),
    NumMarbles =< 6.

% is_player_winning(+Player, +MarblesOnBoard)
% Predicate to check if the player is winning the game
is_player_winning(Player, MarblesOnBoard) :-
    findall((Player, _, _), member((Player, _, _), MarblesOnBoard), PlayerMarbles),
    length(PlayerMarbles, NumMarbles),
    NumMarbles is 7.    

% forced_moves(+Player, +Size, +MarblesOnBoard, -ForcedMoves)
% Predicate to get all the forced moves for a player where it push the opponent marble to out of the board
forced_moves(Player,_,MarblesOnBoard,[]):-
change_player(Player,NewPlayer),
\+ is_player_winning(Player,MarblesOnBoard),
\+ is_player_winning(NewPlayer,MarblesOnBoard).

forced_moves(Player,Size,MarblesOnBoard,ForcedMoves):-
    generate_all_coordinates(Size,Coordinates),
    change_player(Player,Opponent),
    filter_available_moves(Coordinates, MarblesOnBoard, AvailableMoves),
    findall((X, Y), (
        
        member((X,Y), AvailableMoves),
        \+ is_marble_at(_,X,Y,MarblesOnBoard),
        % Simulate placing the marble on the board
        simulate_move(MarblesOnBoard, (Player, X, Y), NewMarblesOnBoard),
        % Check if the opponent would win if they placed a marble at (X, Y)
        has_not_winning_anymore(Opponent, NewMarblesOnBoard)
    ), ForcedMoves).

