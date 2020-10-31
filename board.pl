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
    Empty space.
*/
code(0, '0'). 

/* 
    Red koi.
    Total count: 2 
*/
code(1, 'R'). 

/* 
    Yellow koi.
    Total count: 2
*/
code(2, 'Y').

/* 
    Stone.
    Total count: 20
*/
code(3, 'S').

/* 
    Red counter.
    Total count: 1
*/
code(4, 'r').

/* 
    Yellow counter.
    Total count: 1
*/
code(5, 'y').

/* --------------------------------------- */