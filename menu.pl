:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).

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

menu:-  
    write('Game modes:\n'),
    write('1 - Human vs. Human\n'),
    write('2 - Human vs. Computer\n'),
    write('3 - Computer vs. Computer\n').

set_mode :-
    menu,
    get_option(1, 3, 'Select a mode', Option), !,
    option(Option).