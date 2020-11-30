/* ===== GAME OVER LOGIC ======== */


% game_over(+YellowScore-RedScore, -Player).
% Yellow is the winner.
game_over(_-YellowScore,  2):-
	YellowScore >= 10.

% Red is the winner.
game_over(RedScore-_, 1):-
	RedScore >= 10.
	
	
	
/* ====== SCORING LOGIC ===== */

% Length of the list of Kois with a low enough Delta are adjancent
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

update_scores(1, Board, NewX-NewY, Red-Yellow-NewRed-Yellow):-
	get_adjacent_kois(Board, NewX-NewY, NewScore),
	NewRed is Red + NewScore.
update_scores(2, Board, NewX-NewY, Red-Yellow-Red-NewYellow):-
	get_adjacent_kois(Board, NewX-NewY, NewScore),
	NewYellow is Yellow + NewScore.
	
	
/* ========== TURN LOGIN ========= */

%Turn Conversion table
nextTurn(1,2).
nextTurn(2,1).

		
move(Board-CurrentPlayer-RedRocks-YellowRocks-RedScore-YellowScore, OldX-OldY-NewX-NewY-RockX-RockY, NewBoard-NewCurrentPlayer-NewRedRocks-NewYellowRocks-NewRedScore-NewYellowScore) :-
	move_koi(Board-CurrentPlayer, OldX-OldY-NewX-NewY, _NewBoard),
	nextTurn(CurrentPlayer, NewCurrentPlayer),
	place_rock(_NewBoard-CurrentPlayer-RedRocks-YellowRocks,  OldX-OldY-NewX-NewY, RockX-RockY, NewBoard-NewRedRocks-NewYellowRocks),
	update_scores(CurrentPlayer, NewBoard, NewX-NewY, RedScore-YellowScore-NewRedScore-NewYellowScore).
	

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
	

place_rock(_NewBoard-1-0-YellowRocks, _, n-n, _NewBoard-0-YellowRocks).
place_rock(_NewBoard-2-RedRocks-0, _, n-n, _NewBoard-RedRocks-0).

place_rock(NewBoard-CurrentPlayer-RedRocks-YellowRocks, OldX-OldY-NewX-NewY, n-n, NewBoard-RedRocks-YellowRocks) :-
	cant_place_rock(CurrentPlayer-RedRocks-YellowRocks, [n,n], OldX-OldY-NewX-NewY).
	
place_rock(_NewBoard-CurrentPlayer-RedRocks-YellowRocks, OldX-OldY-NewX-NewY, RockX-RockY, NewBoard-NewRedRocks-NewYellowRocks) :-

	element_at_pos(_NewBoard, RockX-RockY, 0),
	replace_matrix_element(_NewBoard, RockX-RockY, 3, NewBoard),
	can_place_rock(CurrentPlayer, [RedRocks,YellowRocks], OldX-OldY-NewX-NewY),
	decrease_rocks(CurrentPlayer, RedRocks-YellowRocks, NewRedRocks-NewYellowRocks).
	
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
	

cant_place_rock(_, [n,n], OldX-OldY-NewX-NewY)	:-
	DeltaX is NewX-OldX,
	DeltaY is NewY-OldY,
	absolute(DeltaX, AbsX),
	absolute(DeltaY, AbsY),
	Total is AbsX + AbsY,
	\+ can_place_rock_delta_check(Total, AbsX-AbsY).
	

/* ===== HELPER FUNCTIONS ======== */
absolute(X, Abs) :-
	Av is X,
	Av < 0,
	Abs is -Av.
	
absolute(X, Abs) :-
	Av is X,
	Av >= 0,
	Abs is Av.
	
	
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



	

