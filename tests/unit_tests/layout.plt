% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

:- use_module(library(lists)).

% mocks
border_width(1).
border_width_focused(2).
inner_gaps(20).
display(0x7fffffffe2ec).
global_key_value(windows, _, [1, 2, 3, 4]).
global_key_value(monitor_geom, _, [0, 0, 1920, 1080]).
global_key_value(free_win_space, _, [0, 0, 1920, 1080]).
global_key_newvalue(layout, _, Layout) :- layout:is_layout(Layout).
win_properties(1, [managed,  false, false, [200, 200, 300, 600]]).
win_properties(2, [floating, false, false, [0, 30, 50, 50]]).
win_properties(3, [managed,  false, false, [800, 440, 300, 200]]).
win_properties(4, [managed,  true,  false, [1100, 300, 450, 700]]).
plx:x_move_resize_window(Dp, Win, ToX, ToY, ToW, ToH) :-
	integer(Dp), integer(Win),
	integer(ToX), integer(ToY), integer(ToW), integer(ToH),
	0 < Dp, 0 < Win
.
plx:x_configure_window(Dp, Win, VMask, X, Y, W, H, BW, Sib, StackM) :-
	integer(Dp), integer(Win), integer(VMask), integer(X), integer(Y),
	integer(W), integer(H), integer(BW), integer(Sib), integer(StackM),
	0 < Dp, 0 < Win
.

:- begin_tests(layout_tests).

:- use_module("../../src/layout").

test("is_layout +") :-
	assertion(layout:is_layout(floating)),
	assertion(layout:is_layout(monocle)),
	assertion(layout:is_layout(stack)),
	assertion(layout:is_layout(hstack)),
	assertion(layout:is_layout(nrows(N))),
	assertion(layout:is_layout(ncols(N))),
	assertion(layout:is_layout(grid)),
	assertion(layout:is_layout(lmaster)),
	assertion(layout:is_layout(rmaster)),
	assertion(layout:is_layout(tmaster)),
	assertion(layout:is_layout(bmaster)),
	assertion(layout:is_layout(cmaster))
.

test("is_layout -") :-
	assertion(\+ (layout:is_layout(Layout), \+ member(Layout, [
		floating, monocle, stack, hstack, nrows(N), ncols(N), grid,
		lmaster, rmaster, tmaster, bmaster, cmaster
	])))
.

% TODO
%test("set_layout") :-
%.

% TODO
%test("relayout") :-
%.

test("calculate_layout (zero or one window)", [nondet]) :-
	assertion(layout:calculate_layout(_, _, 0, _, [])),

	layout:calculate_layout(_, _, 1, [0, 0, 1024, 768], Geoms1),
	layout:calculate_layout(_, _, 1, [0, 0, 1920, 1080], Geoms2),
	layout:calculate_layout(_, _, 1, [0, 0, 3840, 2160], Geoms3),

	assertion(Geoms1 = [[0, 0, 1020, 764]]), % subtract bigger border 2 x2
	assertion(Geoms2 = [[0, 0, 1916, 1076]]),
	assertion(Geoms3 = [[0, 0, 3836, 2156]])
.

test("calculate_layout (monocle)", [nondet]) :-
	layout:calculate_layout(monocle, _, 3, [0, 0, 1024, 768], Geoms1),
	layout:calculate_layout(monocle, _, 3, [0, 0, 1920, 1080], Geoms2),
	layout:calculate_layout(monocle, _, 3, [0, 0, 3840, 2160], Geoms3),

	assertion(Geoms1 = [[0, 0, 1020, 764], [0, 0, 1020, 764], [0, 0, 1020, 764]]),
	assertion(Geoms2 = [[0, 0, 1916, 1076], [0, 0, 1916, 1076], [0, 0, 1916, 1076]]),
	assertion(Geoms3 = [[0, 0, 3836, 2156], [0, 0, 3836, 2156], [0, 0, 3836, 2156]])
.

test("calculate_layout (stack)", [nondet]) :-
	layout:calculate_layout(stack, _, 3, [0, 0, 1024, 768], Geoms1),
	layout:calculate_layout(stack, _, 3, [0, 0, 1920, 1080], Geoms2),
	layout:calculate_layout(stack, _, 3, [0, 0, 3840, 2160], Geoms3),

	% vertical: inner_gaps 20 x2 + 2x3 border widths = 46, 764-46=718, 718/3 = 239
	assertion(Geoms1 = [[0, 0, 1020, 239], [0, 262, 1020, 239], [0, 525, 1020, 239]]),
	assertion(Geoms2 = [[0, 0, 1916, 343], [0, 366, 1916, 343], [0, 733, 1916, 343]]),
	assertion(Geoms3 = [[0, 0, 3836, 703], [0, 726, 3836, 703], [0, 1453, 3836, 703]])
.

% TODO
%test("calculate_layout (hstack)") :-
%.
%
%% TODO
%test("calculate_layout (nrows)") :-
%.
%
%% TODO
%test("calculate_layout (ncols)") :-
%.
%
%% TODO
%test("calculate_layout (grid)") :-
%.
%
%% TODO
%test("calculate_layout (lmaster)") :-
%.
%
%% TODO
%test("calculate_layout (rmaster)") :-
%.
%
%% TODO
%test("calculate_layout (tmaster)") :-
%.
%
%% TODO
%test("calculate_layout (bmaster)") :-
%.
%
%% TODO
%test("calculate_layout (cmaster)") :-
%.

test("max_border_width +") :-
	assertion(layout:max_border_width(2))
	% see: mocked border_width and border_width_focused
.

test("max_border_width -") :-
	assertion(\+ layout:max_border_width(1))
.

test("stack_xs +") :-
	assertion(layout:stack_xs([10, 20, 30, 40], [44, 20, 30, 40])),
	assertion(layout:stack_xs([0, 0, 0, 0], [4, 0, 0, 0]))
.

test("stack_xs -") :-
	layout:max_border_width(MaxBW),
	assertion(\+ (layout:stack_xs([10, Y, 5, H], [NewX, Y, 5, H]), NewX =\= 10 + 5 + 2 * MaxBW))
.

test("stack_ys +") :-
	assertion(layout:stack_ys([10, 20, 30, 40], [10, 64, 30, 40])),
	assertion(layout:stack_ys([0, 0, 0, 0], [0, 4, 0, 0]))
.

test("stack_ys -") :-
	layout:max_border_width(MaxBW),
	assertion(\+ (layout:stack_ys([X, 10, W, 5], [X, NewY, W, 5]), NewY =\= 10 + 5 + 2 * MaxBW))
.

% TODO
%test("fill_spare_pixels_v +") :-
%	assertion(layout:fill_spare_pixels_v(0, Added, []))
%.

% TODO
%test("fill_spare_pixels_h") :-
%.

% TODO
%test("apply_inner_gaps_v") :-
%.

% TODO
%test("apply_inner_gaps_h") :-
%.

% TODO
%test("apply_geoms") :-
%.

:- end_tests(layout_tests).

