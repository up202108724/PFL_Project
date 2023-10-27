:- use_module(library(lists)).
:- use_module(library(system), [now/1]).
:- consult(dynamicfunctions).
:- consult(board).
% option(+N)
% Game mode options.
option(1):-
    write('Human vs. Human\n'),
    get_name(player1), get_name(player2).
option(2):-
    write('Human vs. Computer\n'),
    get_name(player1),
    asserta((name_of(player2, 'bot'))), !.
option(3):-
    write('Computer vs. Computer\n'),
    asserta((name_of(player1, 'bot1'))),
    asserta((name_of(player2, 'bot2'))), !.

% choose_player(-Player)
% Choose the player who will start the game
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('Who starts playing?\n1 - ~a blue\n2 - ~a red\n', [Name1, Name2]),
    get_option(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player).

% header/0
% Game header
header:-
    write('===============================================\n'),
    write('                                             \n'),
    write('    ( M )( O )( M )( E )( N )( T )( U )( M )        \n'),
    write('                                            \n'),
    write('==============================================\n').

% menu/0
% Main menu
menu:-
    write('Game modes:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Computer\n'),
    write('3 - Computer vs. Computer\n').

% set_mode/0
% Set the game mode
set_mode :-
    menu,
    get_option(1, 3, 'Select a mode', Option), !,
    option(Option).


% get_option(+Min, +Max, +Context, -Value)
% Get a valid option from the user
get_option(Min, Max, Context, Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    read_number(Value),
    (between(Min, Max, Value) -> true ; get_option(Min, Max, Context, Value)).


% read_number(-Number)
% Read a number from the user
read_number(Number) :-
    read(Input),
    (number(Input) -> Number = Input ; write('Invalid input. Please enter a number: '), read_number(Number)).

% get_name(-Player) 
% Get the name of a player 
get_name(Player):-    
       format('Enter the name: ', [Player]),     
       read(Name),     
       asserta((name_of(Player, Name))).  
       
% game_configurations(-GameState) 
% Set the game configurations 
game_configurations([Board,Player,[],0]):-     
       set_mode,      
       choose_player(Player),     
       header.     
    

% game_cycle(-GameState)
% Recursive loop while the game is not over

game_cycle(GameState):-

game_cycle(GameState):-
move(GameState,NewGameState),
game_cycle(NewGameState).

% move(GameState, NewGameState)
% Game action that builds a new GameState, representing a new move on the game 


move(GameState, NewGameState):-
[Board,Player,_,TotalMoves]= GameState,
change_player(Player,NewPlayer),
NewTotalMoves is TotalMoves + 1,
NewGameState = [NewBoard,NewPlayer,NewTotalMoves].

% play
% Starts the game and clears data when it ends

play:-
    game_configurations(GameState), !,
    game_cycle(GameState),
    clear_data.







