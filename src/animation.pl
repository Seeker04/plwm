% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

:- module(animation, []).

:- use_module(library(lists)).

:- use_module(utils).

%! from_to_in_steps(++From:integer, ++To:integer, ++StepCnt:integer, -Steps:[integer]) is det
%
%  Generates a list of equal steps traversing a (From, To] interval (inclusive on the right).
%
%  @arg From start of the interval, always excluded
%  @arg To end of the interval, always included
%  @arg StepCnt count of steps
%  @arg Steps list of equal steps that traverse (From, To]
from_to_in_steps(From, To, StepCnt, Steps) :-
	Distance is To - From,
	Step is round(Distance / StepCnt),
	utils:n_step_list(StepCnt, plus(Step), [From|Rest]),
	append(Rest, [To], Steps),
	!
.

%! merge_geom_lists(++Xs:[integer], ++Ys:[integer], ++Ws:[integer], ++Hs:[integer],
%                   -Merged:[[integer]]) is det
%
%  Zips together four lists of integers to a list of [X,Y,W,H] geometries.
%
%  @arg Xs list of x coordinates
%  @arg Ys list of y coordinates
%  @arg Ws list of widths
%  @arg Hs list of heights
%  @arg Merged list of [X,Y,W,H] geometries
merge_geom_lists([], [], [], [], []).
merge_geom_lists([X|Xs], [Y|Ys], [W|Ws], [H|Hs], [[X, Y, W, H]|Rest]) :-
	merge_geom_lists(Xs, Ys, Ws, Hs, Rest)
.

%! interpolate_geom(++Win:integer, ++X:integer, ++Y:integer, ++W:integer, ++H:integer,
%                   ++NewX:integer, ++NewY:integer, ++NewW:integer, ++NewH:integer,
%                   ++StepCnt:integer, ++Time:float) is det
%
%  Interpolates the specified window's geometry (X, Y, W, H) by applying a series
%  of move-resize operations on it.
%  The number of interpolation steps and its total time are both configurable.
%
%  @arg Win XID of window to operate on
%  @arg X initial x coordinate
%  @arg Y initial y coordinate
%  @arg W initial width
%  @arg H initial height
%  @arg NewX final x coordinate
%  @arg NewY final y coordinate
%  @arg NewW final width
%  @arg NewH final height
%  @arg StepCnt number of interpolation steps (granularity)
%  @arg Time time in seconds the interpolation must take
interpolate_geom(Win, X, Y, W, H, NewX, NewY, NewW, NewH, StepCnt, Time) :-
	user:display(Dp),
	from_to_in_steps(X, NewX, StepCnt, StepsX),
	from_to_in_steps(Y, NewY, StepCnt, StepsY),
	from_to_in_steps(W, NewW, StepCnt, StepsW),
	from_to_in_steps(H, NewH, StepCnt, StepsH),
	merge_geom_lists(StepsX, StepsY, StepsW, StepsH, Steps),
	merge_geom_lists(StepsX, StepsY, StepsW, StepsH, Steps),
	Delay is Time / StepCnt,

	compat_forall(member([ToX, ToY, ToW, ToH], Steps), (
		plx:x_move_resize_window(Dp, Win, ToX, ToY, ToW, ToH),
		plx:x_sync(Dp, false), % flush the event queue
		sleep(Delay)
	))
.
