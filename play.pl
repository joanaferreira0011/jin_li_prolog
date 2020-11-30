:-use_module(library(lists)).
:-use_module(library(random)).

:-include('board.pl').
:-include('input.pl').
:-include('logic.pl').
:-include('ai.pl').



/* ============ PLAY =============  */	
play :- 
	create_board(_X, 10),
	play_loop([[play_human], [play_human]], _X-1-10-10-0-0).
	
	
play_v_bot :-
	create_board(_X, 10),
	play_loop([ [play_human], [choose_move, 0] ], (_X-1-10-10-0-0)).
	
	
play_bot :-
	create_board(_X, 10),
	play_loop([ [choose_move,2], [choose_move,2] ], (_X-1-10-10-0-0)).

	

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
	

			
				
/* ========= HUMAN PLAY ----------- */


play_human(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore,
				ToMoveX-ToMoveY-NewX-NewY-RockX-RockY) :-

	valid_moves(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore, ListOfMoves),
	
	
	findall(X-Y, member(X-Y-_-_-_-_, ListOfMoves), _Koi),
	unique(_Koi, Koi),
	
	choose_list_elem(Koi, 'Choose a Koi to move', KoiIndex),
	nth0(KoiIndex, Koi, ToMove),
	!,
	ToMoveX-ToMoveY = ToMove,
	
	findall(X-Y, member(ToMoveX-ToMoveY-X-Y-_-_, ListOfMoves), _NewPos),
	unique(_NewPos, NewPos),
	choose_list_elem(NewPos, 'Choose a new position', NewPosIndex),
	
	nth0(NewPosIndex, NewPos, NewPosVal),

	NewX-NewY = NewPosVal,
	
	
	
	findall(X-Y, member(ToMoveX-ToMoveY-NewX-NewY-X-Y, ListOfMoves), _ListRocks),
	unique(_ListRocks, ListRocks),
	
	play_human_get_rock(ListRocks, RockX-RockY, Board-CurrentPlayer-ToMoveX-ToMoveY-NewX-NewY),
	
	move(
	Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore,
	ToMoveX-ToMoveY-NewX-NewY-RockX-RockY,
	_
	).
	
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