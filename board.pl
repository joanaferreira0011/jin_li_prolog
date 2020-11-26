:- use_module(library(lists)).
/* ---------- BOARD PIECES ---------- */

/* Empty space. */
code(0, '.'). 

/* Red koi. Total count: 2. */
code(1, 'R'). 
red_koi(1).

/* Yellow koi. Total count: 2. */
code(2, 'Y').
yellow_koi(2).

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
	
	
	
/* ---------- Player Moves ---------- */

own_nth(0, [Element | _Tail], Element).

own_nth(Index, [_CurElement | _Tail], Element) :-
	nonvar(Index),
	NewIndex is Index-1,
	own_nth(NewIndex, _Tail, Element).
	
own_nth(Index, [_CurElement | _Tail], Element) :-
	var(Index), 
	own_nth(NewIndex, _Tail, Element),
	Index is NewIndex+1.

add_current_row(_, [], []).
add_current_row(RowIndex, [X | _Tail], [RowIndex-X | _Rest]) :-
	add_current_row(RowIndex, _Tail, _Rest).
	
	
current_player_koi_accm(_A, _B, _C, KoiAcc, KoiAcc) :-
	length(KoiAcc, 2).

current_player_koi_accm(CurrentPlayer, [Row | NextRows], CurrentRowIndex, Koi, KoiAcc) :-
	findall(X, own_nth(X, Row, CurrentPlayer), Bag),
	add_current_row(CurrentRowIndex, Bag, PlayerKoi),
	
	append(KoiAcc, PlayerKoi, NewAcc),
	NewRow is CurrentRowIndex+1,
	current_player_koi_accm(CurrentPlayer, NextRows, NewRow, Koi, NewAcc).


current_player_koi(CurrentPlayer, Board, Koi) :-
	current_player_koi_accm(CurrentPlayer, Board, 0, Koi, []).
	
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
	
	
choose_list_elem(List, String, Index) :-
	repeat,
	write(String),
	nl,
	print_list_elems(List),
	get_user_input_number(Index),
	own_nth(Index, List, _X).

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

nextTurn(1,2).
nextTurn(2,1).
play_human_v_human(Board, CurrentPlayer, Red-Yellow) :-
	print_game(Board, Red, Yellow),
	findall(X-Y, move_koi(Board-CurrentPlayer, X-Y-_-_, F), _Koi),
	unique(_Koi, Koi),
	choose_list_elem(Koi, 'Choose a Koi to move', KoiIndex),
	own_nth(KoiIndex, Koi, ToMove),
	!,
	ToMoveX-ToMoveY = ToMove,
	findall(X-Y, move_koi(Board-CurrentPlayer, ToMoveX-ToMoveY-X-Y, F), _NewPos),
	
	unique(_NewPos, NewPos),
	choose_list_elem(NewPos, 'Choose a new position', NewPosIndex),
	own_nth(NewPosIndex, NewPos, NewPosVal),
	NewX-NewY = NewPosVal,	
	move_koi(Board-CurrentPlayer, X-Y-NewX-NewY, NewBoard),
	nextTurn(CurrentPlayer, NextTurn),
	play_human_v_human(NewBoard, NextTurn, Red-Yellow).
	
	
/* ---------- GAME STATES ---------- */	
play :- 
	create_board(_X, 10),
	play_human_v_human(_X, 1, 0-0).

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
	print_score('Yell', YellowScore),
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
	
	
small_board([[1,0,1], [0,0,0], [0,1,0]]).
element_at_pos(Board, X-Y, Elem) :-
	own_nth(X, Board, Row),
	own_nth(Y, Row, Elem).

absolute(X, Abs) :-
	Av is X,
	Av < 0,
	Abs is -Av.
	
absolute(X, Abs) :-
	Av is X,
	Av >= 0,
	Abs is Av.

rock_coords(Board, OldX-OldY-NewX-NewY, 2-0-DeltaY, RockX-RockY) :-
	RockX is NewX,
	RockY is (OldY+NewY)//2.

rock_coords(Board, OldX-OldY-NewX-NewY, 2-DeltaX-0, RockX-RockY) :-
	RockX is (OldX+NewX)//2,
	RockY is NewY.
	
rock_coords(Board, OldX-OldY-NewX-NewY, TotalDelta-Delta-Delta, RockX-RockY) :-
	RockX is (OldX+NewX)//2,
	RockY is (OldY+NewY)//2.
	
is_jump_okay( _, _, 1-_-_).
is_jump_okay( _, _, 2-X-X).
is_jump_okay(Board, OldX-OldY-NewX-NewY, TotalDelta-DeltaX-DeltaY) :-
	rock_coords(Board, OldX-OldY-NewX-NewY, TotalDelta-DeltaX-DeltaY, RockX-RockY),
	own_nth(RockX, Board, Row),
	own_nth(RockY, Row, 3).

	
replace_matrix_element([CurrentRow| NextRows], X-Y, NewElem, [CurrentRow | Tail]) :-
	X > 0,
	DecX is X-1,
	replace_matrix_element(NextRows, DecX-Y, NewElem, Tail).

replace_matrix_element([CurrentRow | NextRows], 0-Y, NewElem, [NewRow | NextRows]) :-
	length(CurrentRow, RowLength),
	append(Left, [_ | Right], CurrentRow),
	length(Left, Y),
	append(Left, [NewElem | Right], NewRow).
	
move_koi(Board-CurrentPlayer, OldX-OldY-NewX-NewY, NewBoard) :-
	own_nth(OldX, Board, Row),
	own_nth(OldY, Row, CurrentPlayer),
	own_nth(NewX, Board, NewRow),
	own_nth(NewY, NewRow, 0),
	
	absolute(NewX-OldX, DeltaX),
	absolute(NewY-OldY, DeltaY),
	
	TotalDelta is DeltaX+DeltaY,
	
	member(TotalDelta, [1,2,4]),
	is_jump_okay(Board, OldX-OldY-NewX-NewY, TotalDelta-DeltaX-DeltaY),
	replace_matrix_element(Board, OldX-OldY, 0, _NewBoard),
	replace_matrix_element(_NewBoard, NewX-NewY, CurrentPlayer, NewBoard).


/* GAME SCORE LOGIC */

game_over(YellowScore-RedScore, Winner):-
	YellowScore >= 10,
	Winner is 'Yellow',
	write('Yellow').

game_over(YellowScore-RedScore, Winner):-
	RedScore >= 10,
	Winner is 'Red',
	write('Red').

get_adjacent_kois(Board, Y-X, NumberKois):-
	UpperRow is X-1,
	LowerRow is X+1,
	RightCol is Y+1,
	LeftCol is Y-1,
	get_koi_pos(Board, Y-UpperRow, IsKoiUp),
	get_koi_pos(Board, Y-LowerRow, IsKoiDown),
	get_koi_pos(Board, RightCol-X, IsKoiRight),
	get_koi_pos(Board, LeftCol-X, IsKoiLeft),
	NumberKois is (IsKoiUp + IsKoiDown + IsKoiRight + IsKoiLeft),
	write(NumberKois).

get_koi_pos(Board, Y-X, IsKoi):-
	nth0(X, Board, Row),
	nth0(Y, Row, Elem),
	(Elem == 1; Elem == 2), 
	IsKoi is 1.

get_koi_pos(Board, Y-X, IsKoi):-
	nth0(X, Board, Row),
	nth0(Y, Row, Elem),
	Elem \= 1,
	Elem \= 2, 
	IsKoi is 0.
