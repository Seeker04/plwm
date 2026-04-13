% MIT License, Copyright (c) 2023-2026 Barnabás Zahorán, see LICENSE

% mocks
display(0x7fffffffe2ec).
plx:x_move_resize_window(Dp, Win, ToX, ToY, ToW, ToH) :-
	integer(Dp), integer(Win),
	integer(ToX), integer(ToY), integer(ToW), integer(ToH),
	0 < Dp, 0 < Win
.
plx:x_sync(Dp, Discard) :-
	integer(Dp), 0 < Dp, (Discard = true ; Discard = false)
.

:- begin_tests(animation_tests).

:- use_module("../../src/animation").

test("from_to_in_steps +") :-
	% zero .. zero
	assertion(animation:from_to_in_steps(0, 0, 1, [0])),
	assertion(animation:from_to_in_steps(0, 0, 3, [0, 0, 0])),

	% zero .. positive
	assertion(animation:from_to_in_steps(0, 3, 1, [3])),
	assertion(animation:from_to_in_steps(0, 10, 1, [10])),
	assertion(animation:from_to_in_steps(0, 10, 2, [5, 10])),
	assertion(animation:from_to_in_steps(0, 10, 5, [2, 4, 6, 8, 10])),

	% zero .. negative
	assertion(animation:from_to_in_steps(0, -3, 1, [-3])),
	assertion(animation:from_to_in_steps(0, -10, 1, [-10])),
	assertion(animation:from_to_in_steps(0, -10, 2, [-5, -10])),
	assertion(animation:from_to_in_steps(0, -10, 5, [-2, -4, -6, -8, -10])),

	% positive .. zero
	assertion(animation:from_to_in_steps(3, 0, 1, [0])),
	assertion(animation:from_to_in_steps(10, 0, 1, [0])),
	assertion(animation:from_to_in_steps(10, 0, 2, [5, 0])),
	assertion(animation:from_to_in_steps(10, 0, 5, [8, 6, 4, 2, 0])),

	% negative .. zero
	assertion(animation:from_to_in_steps(-3, 0, 1, [0])),
	assertion(animation:from_to_in_steps(-10, 0, 1, [0])),
	assertion(animation:from_to_in_steps(-10, 0, 2, [-5, 0])),
	assertion(animation:from_to_in_steps(-10, 0, 5, [-8, -6, -4, -2, 0])),

	% negative .. positive
	assertion(animation:from_to_in_steps(-1, 3, 1, [3])),
	assertion(animation:from_to_in_steps(-10, 10, 1, [10])),
	assertion(animation:from_to_in_steps(-10, 10, 2, [0, 10])),
	assertion(animation:from_to_in_steps(-10, 10, 5, [-6, -2, 2, 6, 10])),
	assertion(animation:from_to_in_steps(-100, 200, 3, [0, 100, 200])),

	% positive .. negative
	assertion(animation:from_to_in_steps(1, -3, 1, [-3])),
	assertion(animation:from_to_in_steps(10, -10, 1, [-10])),
	assertion(animation:from_to_in_steps(10, -10, 2, [0, -10])),
	assertion(animation:from_to_in_steps(10, -10, 5, [6, 2, -2, -6, -10])),
	assertion(animation:from_to_in_steps(100, -200, 3, [0, -100, -200]))
.

test("from_to_in_steps -") :-
	assertion(\+ animation:from_to_in_steps(0, 10, 1, [])),
	assertion(\+ animation:from_to_in_steps(0, 10, 1, [_, _|_])),

	assertion(\+ animation:from_to_in_steps(0, 10, 2, [])),
	assertion(\+ animation:from_to_in_steps(0, 10, 2, [_])),
	assertion(\+ animation:from_to_in_steps(0, 10, 2, [_, _, _|_])),

	assertion(\+ (animation:from_to_in_steps(0, 10, 3, [_, _, M]), M \= 10))
.

test("from_to_in_steps - (steps = 0)", [throws(_)]) :-
	animation:from_to_in_steps(_, _, 0, _)
.

test("from_to_in_steps - (steps < 0)") :-
	assertion(\+ animation:from_to_in_steps(0, 10, -1, _)),
	assertion(\+ animation:from_to_in_steps(-1, -5, -2, _))
.

test("merge_geom_lists +") :-
	assertion(animation:merge_geom_lists([], [], [], [], [])),
	assertion(animation:merge_geom_lists([1], [2], [3], [4], [[1, 2, 3, 4]])),
	assertion(animation:merge_geom_lists([1, 2], [10, 20], [100, 200], [1000, 2000],
	                                    [[1, 10, 100, 1000], [2, 20, 200, 2000]])),
	assertion(animation:merge_geom_lists([1, 2, 3], [10, 20, 30], [100, 200, 300], [1000, 2000, 3000],
	                          [[1, 10, 100, 1000], [2, 20, 200, 2000], [3, 30, 300, 3000]]))
.

test("merge_geom_lists -") :-
	assertion(\+ animation:merge_geom_lists([], [], [], [], [_|_])),
	assertion(\+ animation:merge_geom_lists([_], [], [], [], _)),
	assertion(\+ animation:merge_geom_lists([], [_], [], [], _)),
	assertion(\+ animation:merge_geom_lists([], [], [_], [], _)),
	assertion(\+ animation:merge_geom_lists([], [], [], [_], _)),
	assertion(\+ animation:merge_geom_lists([_], [_], [_], [_], [])),
	assertion(\+ animation:merge_geom_lists([_], [_], [_], [_], [_, _|_])),
	assertion(\+ animation:merge_geom_lists([_, _], [_, _], [_, _], [_, _], [])),
	assertion(\+ animation:merge_geom_lists([_, _], [_, _], [_, _], [_, _], [_])),
	assertion(\+ animation:merge_geom_lists([_, _], [_, _], [_, _], [_, _], [_, _, _|_]))
.

test("interpolate_geom +") :-
	%                                    Win    Geom                NewGeom           SCnt Time
	assertion(animation:interpolate_geom(1000,   0,   0,   0,   0,  10,  20,  30,  40, 30, 0.001)),
	assertion(animation:interpolate_geom(1000,  10,  20,  30,  40,   0,   0,   0,   0, 30, 0.001)),
	assertion(animation:interpolate_geom(1000,   0,   0,   0,   0, -10, -20, -30, -40, 30, 0.001)),
	assertion(animation:interpolate_geom(1000, -10, -20, -30, -40,   0,   0,   0,   0, 30, 0.001)),
	assertion(animation:interpolate_geom(1000, -10, -20, -30, -40,  10,  20,  30,  40, 30, 0.001)),
	assertion(animation:interpolate_geom(1000,  10,  20,  30,  40, -10, -20, -30, -40, 30, 0.001))
.

test("interpolate_geom + (time <= 0)") :-
	assertion(animation:interpolate_geom(1000, 0, 0, 0, 0, 10, 20, 30, 40, 30, 0.0)),
	assertion(animation:interpolate_geom(1000, 0, 0, 0, 0, 10, 20, 30, 40, 30, -1.0))
.
% Note: sleep/1 simply returns immediately if its argument is <=0.0, that's why the above don't fail
% check_config/0 will fail however if animation_time not greater than 0.0

test("interpolate_geom - (win <= 0)") :-
	%                                       Win  Geom               NewGeom           SCnt Time
	assertion(\+ animation:interpolate_geom(0,   0,   0,   0,   0,  10,  20,  30,  40, 30, 0.001)),
	assertion(\+ animation:interpolate_geom(-1,  0,   0,   0,   0,  10,  20,  30,  40, 30, 0.001))
.

test("interpolate_geom - (float in geom 1)", [throws(_)]) :-
	animation:interpolate_geom(1000, 1.0, 0, 0, 0, 10, 20, 30, 40, 30, 0.001)
.
test("interpolate_geom - (float in geom 2)", [throws(_)]) :-
	animation:interpolate_geom(1000, 0, 1.0, 0, 0, 10, 20, 30, 40, 30, 0.001)
.
test("interpolate_geom - (float in geom 3)", [throws(_)]) :-
	animation:interpolate_geom(1000, 0, 0, 1.0, 0, 10, 20, 30, 40, 30, 0.001)
.
test("interpolate_geom - (float in geom 4)", [throws(_)]) :-
	animation:interpolate_geom(1000, 0, 0, 0, 1.0, 10, 20, 30, 40, 30, 0.001)
.
test("interpolate_geom - (float in geom 5-8)") :-
	assertion(\+ animation:interpolate_geom(1000, 0, 0, 0, 0,  10.0,  20,  30,    40,   30, 0.001)),
	assertion(\+ animation:interpolate_geom(1000, 0, 0, 0, 0,  10,  20.0,  30,    40,   30, 0.001)),
	assertion(\+ animation:interpolate_geom(1000, 0, 0, 0, 0,  10,    20,  30.0,  40,   30, 0.001)),
	assertion(\+ animation:interpolate_geom(1000, 0, 0, 0, 0,  10,    20,  30,    40.0, 30, 0.001))
.

test("interpolate_geom - (stepcnt = 0)", [throws(_)]) :-
	animation:interpolate_geom(0, 0, 0, 0, 0, 10, 20, 30, 40, 0, 0.001)
.

test("interpolate_geom - (stepcnt < 0)") :-
	assertion(\+ animation:interpolate_geom(0, 0, 0, 0, 0, 10, 20, 30, 40, -1, 0.001))
.

:- end_tests(animation_tests).

