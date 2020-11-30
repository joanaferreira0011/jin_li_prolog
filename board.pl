

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
	

	
print_list_elems_intern([], _).
print_list_elems_intern([X | _Tail], Index) :-
	write(Index),
	write(' - '),
	write(X),
	nl,
	NewIndex is Index+1,
	print_list_elems_intern(_Tail, NewIndex).
	
print_list_elems(L):-
	print_list_elems_intern(L, 0).
	
	

unique([], Unique, Unique).
unique([X | Rest], Unique, Acc) :-
	\+ member(X, Acc),
	append(Acc, [X], NewAcc),
	unique(Rest, Unique, NewAcc).
	
unique([X | Rest], Unique, Acc) :-
	member(X, Acc),
	unique(Rest, Unique, Acc).

unique(List, Unique) :-
	unique(List, Unique, []).






	

	



	
	
/* ---------- BOARD RENDERING ---------- */

print_game(GameState) :-
	Board-_-RedRocks-YellowRocks-RedScore-YellowScore = GameState,
	print_game(Board, RedScore-YellowScore, RedRocks-YellowRocks).
	
print_game([L|T], RedScore-YellowScore, RedRocks-YellowRocks):-
    write('***** JIN LI *****'),
    nl,
    print_board([L|T]),
    nl,
	print_score('R', RedScore-RedRocks),
	print_score('Y', YellowScore-YellowRocks).

print_score(Player, Score-Rocks):-
    write(Player),
	write(' Score: '), write(Score), write('\t'),
	write(' Rocks: '), write(Rocks),
	nl.

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
	
	
element_at_pos(Board, X-Y, Elem) :-
	nth0(X, Board, Row),
	nth0(Y, Row, Elem).

absolute(X, Abs) :-
	Av is X,
	Av < 0,
	Abs is -Av.
	
absolute(X, Abs) :-
	Av is X,
	Av >= 0,
	Abs is Av.


	
replace_matrix_element([CurrentRow| NextRows], X-Y, NewElem, [CurrentRow | Tail]) :-
	X > 0,
	DecX is X-1,
	replace_matrix_element(NextRows, DecX-Y, NewElem, Tail).

replace_matrix_element([CurrentRow | NextRows], 0-Y, NewElem, [NewRow | NextRows]) :-
	append(Left, [_ | Right], CurrentRow),
	length(Left, Y),
	append(Left, [NewElem | Right], NewRow).
	




