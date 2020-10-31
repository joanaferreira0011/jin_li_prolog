/* ---------- BOARD ---------- */
print_tab([]).
print_tab([L|T]):-
    write('| '), print_line(L), nl, print_tab(T).
print_line([]).
print_line([C | L]):-
    print_cell(C),
    print_line(L).
print_cell(C):-
    code(C,P),
    write(P), write(' | ').

/* ---------- BOARD PIECES ---------- */

/* 
    Red koi
    Total count: 2 
*/
code(0, 'R'). 

/* 
    Yellow koi
    Total count: 2
*/
code(1, 'Y').

/* 
    Stone
    Total count: 20
*/
code(2, 'S').

/* 
    Red counter
    Total count: 1
*/
code(3, 'r').

/* 
    Yellow counter
    Total count: 1
*/
code(4, 'y').

/* --------------------------------------- */