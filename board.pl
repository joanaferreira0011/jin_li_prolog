:-use_module(library(lists)).
:-use_module(library(random)).


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
	
match_char(_, _, _, _) :-
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


update_scores(1, Board, NewX-NewY, Red-Yellow-NewRed-Yellow):-
	get_adjacent_kois(Board, NewX-NewY, NewScore),
	NewRed is Red + NewScore.
update_scores(2, Board, NewX-NewY, Red-Yellow-Red-NewYellow):-
	get_adjacent_kois(Board, NewX-NewY, NewScore),
	NewYellow is Yellow + NewScore.


%Does a random move	
choose_move(0, GameState, Move):-
	valid_moves(GameState, ListOfMoves),
	
	length(ListOfMoves, LengthList),
	
	random(0, LengthList, Rand),
	own_nth(Rand, ListOfMoves, Move),
	
	move(GameState, 
	Move,
	_).
choose_move(Level, GameState, Move) :-
	Level > 0,
	minimax(Level, GameState, Move),
	write('Choose_move got '), write(Move), nl.

rank_states(State, Value-State) :-
	value(State, Value).
	
remove_rank_states(_-B, B).
	
get_best_states(States, F) :-
	maplist(rank_states, States, _F),
	sort(_F, _Ftwo),
	length(F, 4),
	append(_, _Ff, _Ftwo),
	maplist(remove_rank_states, _Ff, F).
	
	
	
minimax_choose_best(Level, BestMoves, BestMove) :-
	Type is Level mod 2,
	minimax_choose_best(Level, Type, BestMoves, BestMove).
	
minimax_choose_best(_, 1, BestMoves, BestMove) :-
	min_member(BestMove, BestMoves).
	
minimax_choose_best(_, 0, BestMoves, BestMove) :-
	max_member(BestMove, BestMoves).
	
minimax(Level, Level, Move, GameState, Value-Move):-
	value(GameState, Value).

minimax(Level, MaxLevel, CurrentMove, GameState, BestMove) :-
	Level > 0,
	valid_states(GameState, _ListOfStates),
	!,
	get_best_states(_ListOfStates, ListOfStates),
	NewLevel is Level+1,
	Caller =.. [minimax, NewLevel, MaxLevel, CurrentMove],
	maplist(Caller, ListOfStates, BestMoves),
	!,
	minimax_choose_best(Level, BestMoves, BestMove).



minimax(Level, GameState, Move) :-
	Level > 0,
	valid_all(GameState, _ListOfAll),
	
	random_permutation(_ListOfAll, Permutation),
	append(_, ListOfAll, Permutation),
	length(ListOfAll, 4),
	maplist(get_move_from_all, ListOfAll, ListOfMoves),
	maplist(get_state_from_all, ListOfAll, ListOfStates),
	generate_first_best_moves(Level, ListOfMoves, ListOfStates, BestMoves),
	minimax_choose_best(_, 0, BestMoves, _Move),
	_-(A-B-C-D-E-F) = _Move,
	Move = A-B-C-D-E-F.
	
generate_first_best_moves(_, [], [], []).
generate_first_best_moves(MaxLevel, [CurrentMove | TailMoves], [CurrentState | TailStates], [CurrentBest | TailBests]) :-
	minimax(1, MaxLevel, CurrentMove, CurrentState, CurrentBest),
	generate_first_best_moves(MaxLevel, TailMoves, TailStates, TailBests).
		

get_move_from_all(FromX-FromY-ToX-ToY-RockX-RockY-_, FromX-FromY-ToX-ToY-RockX-RockY).
get_state_from_all(_-_-_-_-_-_-State, State).
	
play_human_get_rock_ask(RockX-RockY) :-
	write('Choose where to drop a rock'), nl,
	get_user_input_number(RockX),
	!,
	get_user_input_number(RockY),
	!.

play_human_get_rock([n-n], n-n, _).
play_human_get_rock(ListRocks, RockX-RockY, Board-CurrentPlayer-OldX-OldY-NewX-NewY) :-
	length(ListRocks, N),
	N > 1,
	move_koi(Board-CurrentPlayer, OldX-OldY-NewX-NewY, _InterBoard),
	print_board(_InterBoard),
	repeat,
	play_human_get_rock_ask(RockX-RockY),	
	member(RockX-RockY, ListRocks).
	
valid_moves(GameState, ListOfMoves) :-
	findall(FromX-FromY-ToX-ToY-RockX-RockY, move(GameState, FromX-FromY-ToX-ToY-RockX-RockY, _), _ListOfMoves),
	unique(_ListOfMoves, ListOfMoves).
	
valid_states(GameState, ListOfStates) :-
	findall(NewState, move(GameState, _, NewState), _ValidStates),
	unique(_ValidStates, ListOfStates).
	
valid_all(GameState, ListOfAll) :-
	findall(FromX-FromY-ToX-ToY-RockX-RockY-NewState, move(GameState, FromX-FromY-ToX-ToY-RockX-RockY, NewState), _ListOfAll),
	unique(_ListOfAll, ListOfAll).
	
	
play_human(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore,
				ToMoveX-ToMoveY-NewX-NewY-RockX-RockY) :-

	valid_moves(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore, ListOfMoves),
	
	
	findall(X-Y, member(X-Y-_-_-_-_, ListOfMoves), _Koi),
	unique(_Koi, Koi),
	
	choose_list_elem(Koi, 'Choose a Koi to move', KoiIndex),
	own_nth(KoiIndex, Koi, ToMove),
	!,
	ToMoveX-ToMoveY = ToMove,
	
	findall(X-Y, member(ToMoveX-ToMoveY-X-Y-_-_, ListOfMoves), _NewPos),
	unique(_NewPos, NewPos),
	choose_list_elem(NewPos, 'Choose a new position', NewPosIndex),
	
	own_nth(NewPosIndex, NewPos, NewPosVal),

	NewX-NewY = NewPosVal,
	
	
	
	findall(X-Y, member(ToMoveX-ToMoveY-NewX-NewY-X-Y, ListOfMoves), _ListRocks),
	unique(_ListRocks, ListRocks),
	
	play_human_get_rock(ListRocks, RockX-RockY, Board-CurrentPlayer-ToMoveX-ToMoveY-NewX-NewY),
	
	move(
	Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore,
	ToMoveX-ToMoveY-NewX-NewY-RockX-RockY,
	_
	).
	

play_loop(_, GameState) :-
	_-_-_-_-RedScore-YellowScore = GameState,
	game_over(RedScore-YellowScore, X),
	print_game(GameState),
	write('Congrats! '), code(X, L), write(L), write(' you won!'),nl.
		
play_loop([CurrentPlay , NextPlay], GameState) :-
	print_game(GameState),
	append(CurrentPlay, [GameState, GameMove], CurrentPlayWithState),
	Caller =.. CurrentPlayWithState,
	Caller,
	write('Estou no loop '), write(GameMove), nl,
	!,
	move(GameState, GameMove, NewGameState),
	write('Whaaaat'), nl,
	play_loop([NextPlay , CurrentPlay], NewGameState).
	

/* ---------- GAME STATES ---------- */	
play :- 
	create_board(_X, 10),
	play_loop([[play_human], [play_human]], _X-1-10-10-0-0).
	
	
play_v_bot :-
	create_board(_X, 10),
	play_loop([ [play_human], [choose_move, 0] ], (_X-1-10-10-0-0)).
	
	
play_bot :-
	create_board(_X, 10),
	play_loop([ [choose_move,2], [choose_move,2] ], (_X-1-10-10-0-0)).

				
	
	
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

rock_coords(_, _-OldY-NewX-NewY, 2-0-_, RockX-RockY) :-
	RockX is NewX,
	RockY is (OldY+NewY)//2.

rock_coords(_, OldX-_-NewX-NewY, 2-_-0, RockX-RockY) :-
	RockX is (OldX+NewX)//2,
	RockY is NewY.
	
rock_coords(_, OldX-OldY-NewX-NewY, _-Delta-Delta, RockX-RockY) :-
	RockX is (OldX+NewX)//2,
	RockY is (OldY+NewY)//2.
	
is_jump_okay( _, _, 1-_-_).
is_jump_okay( _, _, 2-X-X).
is_jump_okay(Board, OldX-OldY-NewX-NewY, TotalDelta-DeltaX-DeltaY) :-
	rock_coords(Board, OldX-OldY-NewX-NewY, TotalDelta-DeltaX-DeltaY, RockX-RockY),
	element_at_pos(Board, RockX-RockY, 3).
	
replace_matrix_element([CurrentRow| NextRows], X-Y, NewElem, [CurrentRow | Tail]) :-
	X > 0,
	DecX is X-1,
	replace_matrix_element(NextRows, DecX-Y, NewElem, Tail).

replace_matrix_element([CurrentRow | NextRows], 0-Y, NewElem, [NewRow | NextRows]) :-
	append(Left, [_ | Right], CurrentRow),
	length(Left, Y),
	append(Left, [NewElem | Right], NewRow).
	
move_koi(Board-CurrentPlayer, OldX-OldY-NewX-NewY, NewBoard) :-
	element_at_pos(Board, OldX-OldY, CurrentPlayer),
	element_at_pos(Board, NewX-NewY, 0),
	
	absolute(NewX-OldX, DeltaX),
	absolute(NewY-OldY, DeltaY),
	
	TotalDelta is DeltaX+DeltaY,
	
	member(TotalDelta, [1,2,4]),
	is_jump_okay(Board, OldX-OldY-NewX-NewY, TotalDelta-DeltaX-DeltaY),
	replace_matrix_element(Board, OldX-OldY, 0, _NewBoard),
	replace_matrix_element(_NewBoard, NewX-NewY, CurrentPlayer, NewBoard).



/* GAME SCORE LOGIC */

% game_over(+YellowScore-RedScore, -Player).
% Yellow is the winner.
game_over(_-YellowScore,  2):-
	YellowScore >= 10.

% Red is the winner.
game_over(RedScore-_, 1):-
	RedScore >= 10.

get_adjacent_kois(Board, X-Y, NumberKois):-
	findall(
	KoiX-KoiY,
	(
	member(FindingKoi, [1,2]),
	element_at_pos(Board, KoiX-KoiY, FindingKoi),
	DeltaX is X-KoiX,
	DeltaY is Y-KoiY,
	absolute(DeltaX, AbsX),
	absolute(DeltaY, AbsY),
	AbsX =< 1,
	AbsY =< 1,
	Total is AbsX + AbsY,
	Total > 0
	),
	Kois),
	length(Kois, NumberKois).

	
decrease_rocks(1, RedRocks-YellowRocks, NewRedRocks-NewYellowRocks):-
	RedRocks > 0,
	NewRedRocks is RedRocks-1,
	NewYellowRocks is YellowRocks.

decrease_rocks(2, RedRocks-YellowRocks, NewRedRocks-NewYellowRocks):-
	YellowRocks > 0,
	NewRedRocks is RedRocks,
	NewYellowRocks is YellowRocks-1.

	
can_place_rock_delta_check(1, _).
can_place_rock_delta_check(2, 1-1).

cant_place_rock(_, [n,n], OldX-OldY-NewX-NewY)	:-
	DeltaX is NewX-OldX,
	DeltaY is NewY-OldY,
	absolute(DeltaX, AbsX),
	absolute(DeltaY, AbsY),
	Total is AbsX + AbsY,
	\+ can_place_rock_delta_check(Total, AbsX-AbsY).

can_place_rock(CurrentPlayer, RocksList, OldX-OldY-NewX-NewY)	:-
	nth1(CurrentPlayer, RocksList, Rocks),
	nonvar(Rocks),
	Rocks \= n,
	Rocks > 0,
	DeltaX is NewX-OldX,
	DeltaY is NewY-OldY,
	absolute(DeltaX, AbsX),
	absolute(DeltaY, AbsY),
	Total is AbsX + AbsY,
	can_place_rock_delta_check(Total, AbsX-AbsY).



	
place_rock(_NewBoard-1-0-YellowRocks, _, n-n, _NewBoard-0-YellowRocks).
place_rock(_NewBoard-2-RedRocks-0, _, n-n, _NewBoard-RedRocks-0).

place_rock(NewBoard-CurrentPlayer-RedRocks-YellowRocks, OldX-OldY-NewX-NewY, n-n, NewBoard-RedRocks-YellowRocks) :-
	cant_place_rock(CurrentPlayer-RedRocks-YellowRocks, [n,n], OldX-OldY-NewX-NewY).
	
place_rock(_NewBoard-CurrentPlayer-RedRocks-YellowRocks, OldX-OldY-NewX-NewY, RockX-RockY, NewBoard-NewRedRocks-NewYellowRocks) :-

	element_at_pos(_NewBoard, RockX-RockY, 0),
	replace_matrix_element(_NewBoard, RockX-RockY, 3, NewBoard),
	can_place_rock(CurrentPlayer, [RedRocks,YellowRocks], OldX-OldY-NewX-NewY),
	decrease_rocks(CurrentPlayer, RedRocks-YellowRocks, NewRedRocks-NewYellowRocks).
	

		
move(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore, OldX-OldY-NewX-NewY-RockX-RockY, NewBoard-NewCurrentPlayer-NewRedRocks-NewYellowRocks-NewRedScore-NewYellowScore) :-
	move_koi(Board-CurrentPlayer, OldX-OldY-NewX-NewY, _NewBoard),
	nextTurn(CurrentPlayer, NewCurrentPlayer),
	place_rock(_NewBoard-CurrentPlayer-RedRocks-YellowRocks,  OldX-OldY-NewX-NewY, RockX-RockY, NewBoard-NewRedRocks-NewYellowRocks),
	update_scores(CurrentPlayer, NewBoard, NewX-NewY, RedScore-YellowScore-NewRedScore-NewYellowScore).
	

% VALUE FUNCTIONS - value(+YellowScore-RedScore, +Player, -Value)
value(GameState, Value) :-
	_-CurrentPlayer-_-_-RedScore-YellowScore = GameState,
	value(RedScore-YellowScore, CurrentPlayer, Value).
	
	
value(RedScore-YellowScore, 2, Value):-
	MaxScore is max(YellowScore, RedScore),
	Value is (YellowScore-RedScore) * MaxScore.

value(RedScore-YellowScore, 1, Value):-
	MaxScore is max(YellowScore, RedScore),
	Value is (RedScore-YellowScore) * MaxScore.


