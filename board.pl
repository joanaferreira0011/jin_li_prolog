

/* ---------- BOARD PIECES ---------- */

/* Empty space. */
code(0, ' '). 

/* Red koi. Total count: 2. */
code(1, 'R'). 

/* Yellow koi. Total count: 2. */
code(2, 'Y').

/* Stone. Total count: 20. */
code(3, '*').




/* ---------- BOARD CREATION ---------- */

/* Creates list with length N and all elements El */
fill([], _, 0).
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

	
/* ---------- BOARD RENDERING ---------- */

print_game(GameState) :-
	Board-_-RedRocks-YellowRocks-RedScore-YellowScore = GameState,
	print_game(Board, RedScore-YellowScore, RedRocks-YellowRocks).
	
print_game([L|T], RedScore-YellowScore, RedRocks-YellowRocks):-

	
	Full = [L | T],
	length(Full, Length),
	print_same_char_nonl(Length, '*'),
    write(' JIN LI '),
	print_same_char_nonl(Length, '*'),
    nl,
	print_board([L|T]),
	print_same_char(4*Length, '-'),
    nl,
	print_score('R', RedScore-RedRocks),
	print_score('Y', YellowScore-YellowRocks).

print_score(Player, Score-Rocks):-
    write(Player),
	write(' Score: '), write(Score), write('\t'),
	write(' Rocks: '), write(Rocks),
	nl.

print_board(B) :-
	length(B, Length),
	print_board(B, 4*Length).
	
print_board([], _).
print_board([L|T], Length):-
	print_same_char(Length, '-'),
    write('| '), print_line(L), nl, print_board(T, Length).

print_line([]).
print_line([C | L]):-
    print_cell(C),
    print_line(L).

print_cell(C):-
    code(C,P),
    write(P), write(' | ').

%prints same char multiple times	
print_same_char(N, Char) :-
	print_same_char_nonl(N, Char),
	nl.
	
print_same_char_nonl(0, _).
print_same_char_nonl(N, Char) :-
	NewN is N-1,
	write(Char),
	print_same_char_nonl(NewN, Char).
	
/* ==== BOARD ELEMENT RETRIEVAL ===== */
element_at_pos(Board, X-Y, Elem) :-
	nth0(X, Board, Row),
	nth0(Y, Row, Elem).


/* ====== BOARD ELEMENT REPLACEMENT ======== */
replace_matrix_element([CurrentRow| NextRows], X-Y, NewElem, [CurrentRow | Tail]) :-
	X > 0,
	DecX is X-1,
	replace_matrix_element(NextRows, DecX-Y, NewElem, Tail).

replace_matrix_element([CurrentRow | NextRows], 0-Y, NewElem, [NewRow | NextRows]) :-
	append(Left, [_ | Right], CurrentRow),
	length(Left, Y),
	append(Left, [NewElem | Right], NewRow).