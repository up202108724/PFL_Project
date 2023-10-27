board(10, [
        [empty,     empty,      empty,     empty,     lion1,     lion1,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     elephant1, mouse1,    mouse1,    elephant1, empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     empty,     empty,     empty,     empty,     empty,     empty],
        [empty,     empty,      empty,     elephant2, mouse2,    mouse2,    elephant2, empty,     empty,     empty],
        [empty,     empty,      empty,     empty,     lion2,     lion2,     empty,     empty,     empty,     empty]
]).

change_player(player1, player2).
change_player(player2, player1). 

% change_player(+CurrentPlayer,-NextPlayer) 
% Change player turn