/* ===== MOVE GENERATOR ======== */

%only moves
valid_moves(GameState, ListOfMoves) :-
	findall(FromX-FromY-ToX-ToY-RockX-RockY, move(GameState, FromX-FromY-ToX-ToY-RockX-RockY, _), _ListOfMoves),
	unique(_ListOfMoves, ListOfMoves).

%only states
valid_states(GameState, ListOfStates) :-
	findall(NewState, move(GameState, _, NewState), _ValidStates),
	unique(_ValidStates, ListOfStates).
	
	
%everything - optimization for minimax	
valid_all(GameState, ListOfAll) :-
	findall(FromX-FromY-ToX-ToY-RockX-RockY-NewState, move(GameState, FromX-FromY-ToX-ToY-RockX-RockY, NewState), _ListOfAll),
	unique(_ListOfAll, ListOfAll).
	
	

%Does a random move	
choose_move(0, GameState, Move):-
	valid_moves(GameState, ListOfMoves),
	
	length(ListOfMoves, LengthList),
	
	random(0, LengthList, Rand),
	nth0(Rand, ListOfMoves, Move),
	
	move(GameState, 
	Move,
	_).
	
	
choose_move(Level, GameState, Move) :-
	Level > 0,
	minimax(Level, GameState, Move).


	
	
/* ====== MINIMAX ========= */


/*

	*_choose_best decide when they should maximize or minimize and
	act accordingly
	
*/
minimax_choose_best(Level, BestMoves, BestMove) :-
	Type is Level mod 2,
	minimax_choose_best(Level, Type, BestMoves, BestMove).
	
minimax_choose_best(_, 1, BestMoves, BestMove) :-
	min_member(BestMove, BestMoves).
	
minimax_choose_best(_, 0, BestMoves, BestMove) :-
	max_member(BestMove, BestMoves).
	
	
%bottom case, we've reached the maximum depth
minimax(Level, Level, Move, GameState, Value-Move):-
	value(GameState, Value).

%walks through the tree pruning some states
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


%entrypoint for minimax
minimax(Level, GameState, Move) :-
	Level > 0,
	valid_all(GameState, _ListOfAll),
	
	random_permutation(_ListOfAll, Permutation),
	append(_, ListOfAll, Permutation),
	length(ListOfAll, 4), %initial pruning, mandatory for performance
	maplist(get_move_from_all, ListOfAll, ListOfMoves),
	maplist(get_state_from_all, ListOfAll, ListOfStates),
	generate_first_best_moves(Level, ListOfMoves, ListOfStates, BestMoves),
	minimax_choose_best(_, 0, BestMoves, _Move),
	_-(A-B-C-D-E-F) = _Move,
	Move = A-B-C-D-E-F.

%walks the moves and states linearly and to perform a DFS by calling minimax on each root child
generate_first_best_moves(_, [], [], []).
generate_first_best_moves(MaxLevel, [CurrentMove | TailMoves], [CurrentState | TailStates], [CurrentBest | TailBests]) :-
	minimax(1, MaxLevel, CurrentMove, CurrentState, CurrentBest),
	generate_first_best_moves(MaxLevel, TailMoves, TailStates, TailBests).
		

get_move_from_all(FromX-FromY-ToX-ToY-RockX-RockY-_, FromX-FromY-ToX-ToY-RockX-RockY).
get_state_from_all(_-_-_-_-_-_-State, State).

%helps sort the states
rank_states(State, Value-State) :-
	value(State, Value).

%ranks must be removed for playability
remove_rank_states(_-B, B).
	
get_best_states(States, F) :-
	maplist(rank_states, States, _F),
	sort(_F, _Ftwo),
	length(F, 4), %4 is the branch degree, even for this problem it's a lot!
	append(_, _Ff, _Ftwo),
	maplist(remove_rank_states, _Ff, F).


/* ========= VALUE FUNCTIONS ======= */

% VALUE FUNCTIONS - value(+YellowScore-RedScore, +Player, -Value)
% Heuristic subtracts the koi total area to promote them coming together
value(GameState, Value) :-
	Board-CurrentPlayer-_-_-RedScore-YellowScore = GameState,
	
	findall(X-Y, element_at_pos(Board, X-Y, CurrentPlayer), Kois),
	[Fx-Fy, Sx-Sy] = Kois,
	DeltaX is Fx-Sx,
	DeltaY is Fy-Sy,
	STotal is DeltaX*DeltaY,
	absolute(STotal, _Total),
	max_member(Total, [_Total, 1]),
	
	value(RedScore-YellowScore, CurrentPlayer, V),
	Value is V-Total.
	
	
value(RedScore-YellowScore, 2, Value):-
	MaxScore is max(YellowScore, RedScore),
	Value is (YellowScore-RedScore) * MaxScore.

value(RedScore-YellowScore, 1, Value):-
	MaxScore is max(YellowScore, RedScore),
	Value is (RedScore-YellowScore) * MaxScore.
