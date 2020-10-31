/* ---------- BOARD PIECES ---------- */

/* Empty space. */
code(0, '.'). 

/* Red koi. Total count: 2. */
code(1, 'R'). 

/* Yellow koi. Total count: 2. */
code(2, 'Y').

/* Stone. Total count: 20. */
code(3, '*').


/* ---------- BOARD ---------- */
print_game([L|T], YellowScore, RedScore):-
    write('***** JIN LI *****'),
    nl,
    print_board([L|T]),
    nl,
    print_score('Yellow', YellowScore),
    print_score('Red', RedScore).

print_score(Player, Score):-
    write(Player), write('\'s score: '), write(Score), nl.

print_board([]).
print_board([L|T]):-
    write('| '), print_line(L), nl, print_board(T).

print_line([]).
print_line([C | L]):-
    print_cell(C),
    print_line(L).

print_cell(C):-
    code(C,P),
    write(P), write(' | ').
