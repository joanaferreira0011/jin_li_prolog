/* ---------- BOARD PIECES ---------- */

/* Empty space. */
code(0, '.'). 

/* Red koi. Total count: 2. */
code(1, 'R'). 

/* Yellow koi. Total count: 2. */
code(2, 'Y').

/* Stone. Total count: 20. */
code(3, '*').


fill([], El, 0).
fill([El | T], El, N) :- 
	N > 0,
	Nn is N-1,
	fill(T, El, Nn).
	
create_empty_row(X, N) :-
		fill(X, 0, N).

create_middle_board(X, L) :-
	create_empty_row(_Y, L),
	Middle is L-2,
	fill(X, _Y, Middle).

create_player_row(X, Between, P) :-
	_Y = [P | Between],
	append(_Y, [P], X).

/* Start board */
create_board(X, L) :-
	L > 6,
	create_middle_board(_MiddleBoard, L),
	Middle is L-2,
	create_empty_row(_BetweenRow, Middle),
	create_player_row(_RedRow, _BetweenRow, 1),
	create_player_row(_YellowRow, _BetweenRow, 2),
	_AlmostFinal = [ _RedRow | _MiddleBoard],
	append(_AlmostFinal, [_YellowRow], X).
	


/* ---------- BOARD ---------- */

play :- 
	create_board(_X, 7),
	print_game(_X, 0, 0).
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
