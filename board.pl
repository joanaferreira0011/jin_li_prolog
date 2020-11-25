/* ---------- BOARD PIECES ---------- */

/* Empty space. */
code(0, '.'). 

/* Red koi. Total count: 2. */
code(1, 'R'). 

/* Yellow koi. Total count: 2. */
code(2, 'Y').

/* Stone. Total count: 20. */
code(3, '*').


/* ---------- BOARD CREATION ---------- */

/* Creates list with length N and all elements El */
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

create_board(X, L) :-
	L > 6,
	create_middle_board(_MiddleBoard, L),
	Middle is L-2,
	create_empty_row(_BetweenRow, Middle),
	create_player_row(_RedRow, _BetweenRow, 1),
	create_player_row(_YellowRow, _BetweenRow, 2),
	_AlmostFinal = [ _RedRow | _MiddleBoard],
	append(_AlmostFinal, [_YellowRow], X).
	
	
/* ---------- INPUT ---------- */

char_number(0, '0').
char_number(1, '1').
char_number(2, '2').
char_number(3, '3').
char_number(4, '4').
char_number(5, '5').
char_number(6, '6').
char_number(7, '7').
char_number(8, '8').
char_number(9, '9').

match_char(Cur, X, X, [_ | _]) :-
	Cur == '\n'.
	
match_char(Cur, Acc, X, ListRead) :-
	char_number(Int, Cur),
	NewVal is Acc*10 + Int,
	append(ListRead, [Int], NewList),
	get_user_input_number_int(NewVal, X, NewList).
	
match_char(Cur, _, _, _) :-
	fail.

get_user_input_number_int(Acc, X, ListRead) :-
	get_char(_Cur),
	match_char(_Cur, Acc, X, ListRead).

get_user_input_number(X):-
	repeat,
	get_user_input_number_int(0, X, []).
	
get_user_play(X,Y) :-
	write('Coords fam'),
	nl,
	get_user_input_number(X),
	get_user_input_number(Y).
	
	
	
/* ---------- GAME STATES ---------- */


	
play :- 
	create_board(_X, 7),
	print_game(_X, 0, 0),
	get_user_play(X, Y),
	write(X),
	nl,
	write(Y).

middleBoard([
					[3, 0, 0, 0, 0, 0, 0],
					[0, 0, 0, 0, 0, 0, 0],
					[0, 0, 3, 0, 0, 0, 3],
					[0, 0, 0, 2, 1, 0, 0],
					[0, 0, 0, 3, 0, 2, 0],
					[0, 1, 0, 0, 0, 0, 0],
					[0, 0, 0, 0, 0, 0, 3]
				]).
				
middle :-
	middleBoard(_X),
	print_game(_X, 3 ,4).

	
endBoard([
					[3, 0, 0, 3, 0, 0, 0],
					[0, 0, 0, 0, 0, 3, 0],
					[0, 3, 3, 0, 0, 0, 3],
					[3, 1, 3, 0, 3, 0, 0],
					[0, 0, 0, 3, 3, 0, 0],
					[0, 0, 0, 1, 2, 0, 3],
					[3, 0, 0, 2, 3, 0, 3]
				]).
				
end :-
	endBoard(_X),
	print_game(_X, 10, 8).
	
	
/* ---------- BOARD RENDERING ---------- */
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
