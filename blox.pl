:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').
:- ensure_loaded('util.pl').  % Include funcțiile utilitare

% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4

% The state is represented as state(Cells, BlockPos, BlockOrientation)
% where Cells is a list of tiles, BlockPos is the position of the block,
% and BlockOrientation is either 'vertical' or 'horizontal'.
empty_state(state([], (0, 0), vertical)).

% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.

% state is updated by adding tile(Pos) to the list of Cells.
set_tile(state(Cells, BlockPos, BlockOrientation), Pos, state([tile(Pos) | Cells], BlockPos, BlockOrientation)).

% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.

% it does nothing and simply returns the state as-is.
set_blank(S, _, S).

% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).

% state is updated by adding target(Pos) to the list of Cells.
set_target(state(Cells, BlockPos, BlockOrientation), Pos, state([target(Pos) | Cells], BlockPos, BlockOrientation)).

% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.

% state is updated by adding fragile(Pos) to the list of Cells
set_fragile(state(Cells, BlockPos, BlockOrientation), Pos, state([fragile(Pos) | Cells], BlockPos, BlockOrientation)).

% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.

% state is updated by setting the block's initial position to Pos
% and marking it as vertical.
set_block_initial(state(Cells, _, _), Pos, state([tile(Pos) | Cells], Pos, vertical)).

% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]

% Uses pattern matching to determine the block's orientation.
get_b_pos(state(_, (X, Y), vertical), (X, Y)).
get_b_pos(state(_, (X, Y), horizontal), [(X, Y), (X1, Y)]) :- X1 is X + 1.

% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.

% Uses findall to gather all X and Y coordinates from the Cells list,
% then min_list and max_list to determine the bounds.
get_bounds(state(Cells, _, _), Xmin, Xmax, Ymin, Ymax) :-
    findall(X, (member(Cell, Cells), arg(1, Cell, (X, _))), Xs),
    findall(Y, (member(Cell, Cells), arg(1, Cell, (_, Y))), Ys),
    min_list(Xs, Xmin),
    max_list(Xs, Xmax),
    min_list(Ys, Ymin),
    max_list(Ys, Ymax).

% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.

% Uses member to check if Pos matches the position in any of the cells in the state.
get_cell(state(Cells, _, _), Pos, Type) :-
    member(Cell, Cells),
    cell_type(Cell, Pos, Type).

% Helper predicate to determine the type of a cell
% Uses pattern matching to bind Type to the correct tile type.
cell_type(tile(Pos), Pos, tile).
cell_type(fragile(Pos), Pos, fragile).
cell_type(target(Pos), Pos, target).
cell_type(oswitch(Pos), Pos, oswitch).
cell_type(xswitch(Pos), Pos, xswitch).

% move/3
% move(+S, +Move, -SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă blocul a ajuns la scop).

% There are different cases depending on whether the block is vertical or horizontal,
% and whether it remains in the same orientation or changes.
move(state(Cells, (X, Y), vertical), Move, state(Cells, (NX1, NY1), horizontal)) :-
    neighbor((X, Y), Move, (NX1, NY1)),
    neighbor2((X, Y), Move, (NX2, NY2)),
    valid_move(state(Cells, (X, Y), vertical), [(NX1, NY1), (NX2, NY2)]).

move(state(Cells, (X1, Y1), horizontal), Move, state(Cells, (NX1, NY1), horizontal)) :-
    neighbor((X1, Y1), Move, (NX1, NY1)),
    X2 is X1 + 1,
    neighbor((X2, Y1), Move, (NX2, NY2)),
    valid_move(state(Cells, (X1, Y1), horizontal), [(NX1, NY1), (NX2, NY2)]).

move(state(Cells, (X1, Y1), horizontal), Move, state(Cells, (NX, NY), vertical)) :-
    neighbor((X1, Y1), Move, (NX, NY)),
    X2 is X1 + 1,
    valid_move(state(Cells, (X1, Y1), horizontal), [(NX, NY)]).

move(state(Cells, (X, Y), vertical), Move, state(Cells, (NX, NY), vertical)) :-
    neighbor((X, Y), Move, (NX, NY)),
    valid_move(state(Cells, (X, Y), vertical), [(NX, NY)]).

move(state(Cells, (X1, Y1), horizontal), Move, state(Cells, (NX, NY), vertical)) :-
    neighbor((X1, Y1), Move, (NX, NY)),
    X2 is X1 + 1,
    valid_move(state(Cells, (X1, Y1), horizontal), [(NX, NY)]).

% Checks if all positions in Positions are valid tiles.
valid_move(state(Cells, _, _), Positions) :-
    forall(member(Pos, Positions), memberchk(tile(Pos), Cells)).

% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).

% Checks if the block's position matches the target position.
is_final(state(Cells, (X, Y), vertical)) :-
    member(target((X, Y)), Cells).

% pentru etapa 2
% set_switch(+S, +Pos, +Switch, +Func, +Positions, SOut)
% Switch: o sau x
% Func: switch, uponly sau dnonly
% Position: pozitiile podului
set_switch(S, P, _, _, _, SNew) :- set_tile(S, P, SNew).
