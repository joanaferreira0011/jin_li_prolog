/* ============ INPUT =========== */

%Number input translation table
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

%match_char reads every single character until a new line is found
match_char(Cur, X, X, [_ | _]) :-
	Cur == '\n'.
	
match_char(Cur, Acc, X, ListRead) :-
	char_number(Int, Cur),
	NewVal is Acc*10 + Int,
	append(ListRead, [Int], NewList),
	get_user_input_number_int(NewVal, X, NewList).

%if the input is not a number then force failure
match_char(_, _, _, _) :-
	fail.

get_user_input_number_int(Acc, X, ListRead) :-
	get_char(_Cur),
	match_char(_Cur, Acc, X, ListRead).

get_user_input_number(X):-
	repeat,
	get_user_input_number_int(0, X, []).
	
	
/* ====== LIST ELEMENTS ========== */

%prints a list in the format Index - Element
%and blocks until the user inputs a correct index
choose_list_elem(List, String, Index) :-
	repeat,
	write(String),
	nl,
	print_list_elems(List),
	get_user_input_number(Index),
	nth0(Index, List, _X).
	
	
print_list_elems(L):-
	print_list_elems_intern(L, 0).
	
print_list_elems_intern([], _).
print_list_elems_intern([X | _Tail], Index) :-
	write(Index),
	write(' - '),
	write(X),
	nl,
	NewIndex is Index+1,
	print_list_elems_intern(_Tail, NewIndex).

