:-use_module(library(lists)).
:-use_module(library(between)).

:- dynamic name_of/2.

% option(+Option)
% Set the game mode
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

% choose_board_size(+MinSize, -Size)
% Set the board size
choose_board_size(MinSize, Size) :-
    write('Enter the board size (minimum 7): '),
    repeat,
    (
    read_number(Size),
    (Size >= MinSize -> ! ; write('Invalid size, please enter a size of at least 7.'), fail)
    ).



% get_option(+Min, +Max, +Context, -Value)
% Get a valid option from the user
get_option(Min, Max, Context, Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    (
        read_number(Value),
        (between(Min, Max, Value) -> true ;
        write('Invalid input. Please enter a valid number between '), write(Min), write(' and '), write(Max), nl,
        fail)
    ).
% read_number(-Number)
% Read a number from the user
read_number(Number) :-
    read(Input),
    (number(Input) -> Number = Input ; write('Invalid input. Please enter a number: '), read_number(Number)).

% get_name(-Player) 
% Get the name of a player 
get_name(Player):-
    format('Enter the name ', [Player]),
    get_line(Name, []),
    asserta(name_of(Player, Name)).


% get_line(-Result,+Acc)
% Unifies Result with an input line up to endline '\n'
get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1).
get_line(Result, Acc):-
    atom_chars(Result, Acc).

% is_bot(+Player)
% Checks if the player is a bot

is_bot(Player) :-
    name_of(Player,Name),
    member(Name, ['bot', 'bot1', 'bot2']).  
is_bot_name(Name) :-
    member(Name, ['bot', 'bot1', 'bot2']).                