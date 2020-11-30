:-use_module(library(lists)).
:-use_module(library(random)).

:-include('board.pl').
:-include('input.pl').
:-include('logic.pl').
:-include('ai.pl').



/* ============ PLAY =============  */	
play :- 
	repeat,
	write('How big do you want the board?(must be at least 7x7)'), nl,
	get_user_input_number(BoardSize),
	BoardSize > 6,
	create_board(Board, BoardSize),
	
	Options = ['Human', 'Bot'],
	choose_list_elem(Options, 'Who should start?', StartIndex),
	get_player(StartIndex, FirstPlayer),
	choose_list_elem(Options, 'Who should go second?', SecondIndex),
	get_player(SecondIndex, SecondPlayer),
	
	play_loop([FirstPlayer, SecondPlayer], Board-1-10-10-0-0).

get_player(0, [play_human]).
get_player(1, [choose_move, Difficulty]) :-
	repeat,
	write('How smart you want the AI ? (max 4)'), nl,
	get_user_input_number(Difficulty),
	Difficulty < 5.
	
%the play loops is responsible for switching player turns
%it's an agnostic way of having any two oponents
play_loop(_, GameState) :-
	_-_-_-_-RedScore-YellowScore = GameState,
	game_over(RedScore-YellowScore, X),
	print_game(GameState),
	write('Congrats! '), code(X, L), write(L), write(' you won!'),nl.

%univ operator is used to seamless inject arguments
play_loop([CurrentPlay , NextPlay], GameState) :-
	print_game(GameState),
	append(CurrentPlay, [GameState, GameMove], CurrentPlayWithState),
	Caller =.. CurrentPlayWithState,
	Caller,
	!,
	move(GameState, GameMove, NewGameState),
	play_loop([NextPlay , CurrentPlay], NewGameState).
	

			
				
/* ========= HUMAN PLAY ----------- */


play_human(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore,
				ToMoveX-ToMoveY-NewX-NewY-RockX-RockY) :-

	%as an optimization gather all possible movements, it's in the 300-400 area and decreases exponentially
	valid_moves(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore, ListOfMoves),
	
	%after that just reduce the projection area to get the player's intention move
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
	
	
	%Gather the final rock moves
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

%case when player jumps or is lacking rocks
play_human_get_rock([n-n], n-n, _).

%asks the user for the desired rock location, also prints the board to aid
play_human_get_rock(ListRocks, RockX-RockY, Board-CurrentPlayer-OldX-OldY-NewX-NewY) :-
	length(ListRocks, N),
	N > 1,
	move_koi(Board-CurrentPlayer, OldX-OldY-NewX-NewY, _InterBoard),
	
	%prints the board to make it easy to decide
	print_board(_InterBoard),
	repeat,
	play_human_get_rock_ask(RockX-RockY),	
	member(RockX-RockY, ListRocks).