% MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

:- module(animation, []).

from_to_in_steps(From, To, StepCnt, Steps) :-
	Distance is To - From,
	Step is round(Distance / StepCnt),
	utils:n_step_list(StepCnt, plus(Step), [From|Rest]),
	append(Rest, [To], Steps)
.

merge_geom_lists([], [], [], [], []).
merge_geom_lists([X|Xs], [Y|Ys], [W|Ws], [H|Hs], [[X, Y, W, H]|Rest]) :-
	merge_geom_lists(Xs, Ys, Ws, Hs, Rest)
.

interpolate_geom(Win, X, Y, W, H, NewX, NewY, NewW, NewH, StepCnt, Time) :-
	display(Dp),
	from_to_in_steps(X, NewX, StepCnt, StepsX),
	from_to_in_steps(Y, NewY, StepCnt, StepsY),
	from_to_in_steps(W, NewW, StepCnt, StepsW),
	from_to_in_steps(H, NewH, StepCnt, StepsH),
	merge_geom_lists(StepsX, StepsY, StepsW, StepsH, Steps),
	merge_geom_lists(StepsX, StepsY, StepsW, StepsH, Steps),
	Delay is Time / StepCnt,

	forall(member([ToX, ToY, ToW, ToH], Steps), (
		plx:x_move_resize_window(Dp, Win, ToX, ToY, ToW, ToH),
		plx:x_sync(Dp, false), % flush the event queue
		sleep(Delay)
	))
.

