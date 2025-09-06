% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

:- module(layout, []).

:- use_module(library(lists)).

:- use_module(animation).
:- use_module(utils).

% Supported layouts: (feel free to implement your own)

%! is_layout(+Layout:term) is semidet
%
%  Checks if a term is a valid layout.
%
%  @arg Layout term that is a valid layout
is_layout(Layout) :- member(Layout, [
	floating,  % No window is managed automatically, user is responsible for moving/resizing them
	monocle,   % Only the focused window is visible with maximum size (bar areas are preserved)
	stack,     % Vertical stack, all windows have the same size
	hstack,    % Horizontal stack, all windows have the same size
	nrows(N),  % Number of rows is fixed, windows will be spread evenly among rows
	ncols(N),  % Number of columns is fixed, windows will be spread evenly among rows
	grid,      % sqrt(WinCnt) rows and cols (perfect grid with square numbers, something similar otherwise)
	lmaster,   % Master windows are on the left in a stack, rest are on the right in a stack
	rmaster,   % Master windows are on the right in a stack, rest are on the left in a stack
	tmaster,   % Master windows are on the top in a hstack, rest are on the bottom in a hstack
	bmaster,   % Master windows are on the bottom in a hstack, rest are on the top in a hstack
	cmaster    % Master windows are on the center in a stack, rest are in left and right stacks
]).

%! set_layout(++Layout:term) is det
%
%  Sets the layout of the active monitor-workspace.
%
%  @arg Layout layout to set
set_layout(Layout) :- user:active_mon_ws(ActMon, ActWs), set_layout(ActMon-ActWs, Layout).

%! set_layout(++Mon-Ws:string-atom, ++Layout:term) is det
%
%  Sets the layout of the specified monitor-workspace.
%
%  @arg Mon-Ws monitor and workspace name to set the layout on
%  @arg Layout layout to set
set_layout(Mon-Ws, Layout) :-
	(is_layout(Layout) ->
		user:display(Dp), global_key_value(windows, Mon-Ws, Wins),
		(Layout \= floating ->
			global_key_value(free_win_space, Mon, Bounds),
			findall(Win,               % skip unmanaged and fullscreen windows
			       (member(Win, Wins), user:win_properties(Win, [managed, false|_])),
			       ManagedWins),
			length(ManagedWins, WinCnt),
			calculate_layout(Layout, Mon, WinCnt, Bounds, Geoms),
			apply_geoms(ManagedWins, Geoms)
		; true),
		global_key_newvalue(layout, Mon-Ws, Layout),

		% This is needed for restoring sizes of floating wins that exited fullscreen
		compat_forall((member(Win, Wins), user:win_properties(Win, [State, false, [X, Y, W, H]])), (
			((Layout = floating ; State = floating) ->
				plx:x_move_resize_window(Dp, Win, X, Y, W, H)
			; true)
		)),

		% Make fullscreen windows max size (covering bars too) and also raise them
		global_key_value(monitor_geom, Mon, [MX, MY, MW, MH]),
		compat_forall((member(Win, Wins), user:win_properties(Win, [_, true|_])), (
			plx:x_move_resize_window(Dp, Win, MX, MY, MW, MH),
			CWStackMode is 1 << 6, Above is 0,
			plx:x_configure_window(Dp, Win, CWStackMode, 0, 0, 0, 0, 0, 0, Above)
		))
	;
		utils:warn_invalid_arg("set_layout", Layout))
.

%! relayout() is det
%
%  Recalculates and redraws the layout on the active monitor-workspace.
relayout :- global_value(layout, Layout), set_layout(Layout).

%! relayout(++Mon-Ws:string-atom) is det
%
%  Recalculates and redraws the layout on the specified monitor-workspace.
relayout(Mon-Ws) :- global_key_value(layout, Mon-Ws, Layout), set_layout(Mon-Ws, Layout).

%! calculate_layout(++Layout:term, ++Mon:string, ++WinCnt:integer, ++Bounds:[integer], --Geoms:[[integer]]) is det
%
%  Calculates the window geometries based on the desired layout, monitor, window count
%  and reserved screen space.
%
%  @arg Layout the layout to calculate geometries for
%  @arg Mon name of subject monitor
%  @arg WinCnt number of managed windows in the layout
%  @arg Bounds geometry of reserved space on the screen (i.e. screen size minus bars)
%  @arg Geoms list of window geometries resulting from the layout calculation
calculate_layout(_, _, 0, _, []) :- !.

calculate_layout(Layout, Mon, 1, Bounds, Geoms) :- Layout \= monocle, !, calculate_layout(monocle, Mon, 1, Bounds, Geoms).

calculate_layout(monocle, _, WinCnt, [BX, BY, BW, BH], Geoms) :-
	max_border_width(BorderW),
	WinW is BW - 2 * BorderW,
	WinH is BH - 2 * BorderW,
	utils:n_item_clones(WinCnt, [BX, BY, WinW, WinH], Geoms)
.

calculate_layout(stack, _, WinCnt, [BX, BY, BW, BH], Geoms) :-
	max_border_width(BorderW),
	BHminusBorders is BH - 2 * BorderW * WinCnt,
	WinW is BW - 2 * BorderW,
	WinH is floor(BHminusBorders / WinCnt),

	utils:n_step_list(WinCnt, layout:stack_ys, [[BX, BY, WinW, WinH]|GeomsTail]),

	SP is BHminusBorders - WinH * WinCnt,
	fill_spare_pixels_v(SP, 0, [[BX, BY, WinW, WinH]|GeomsTail], AdjustedGeoms),

	inner_gaps(GapPixel),
	(GapPixel = 0 ->
		Geoms = AdjustedGeoms
	;
		TotalSub is GapPixel * (WinCnt - 1),
		SizeSub is floor(TotalSub / WinCnt),
		layout:apply_inner_gaps_v(0, SizeSub, WinCnt, AdjustedGeoms, Geoms)
	)
.

calculate_layout(hstack, _, WinCnt, [BX, BY, BW, BH], Geoms) :-
	max_border_width(BorderW),
	BWminusBorders is BW - 2 * BorderW * WinCnt,
	WinW is floor(BWminusBorders / WinCnt),
	WinH is BH - 2 * BorderW,

	utils:n_step_list(WinCnt, layout:stack_xs, [[BX, BY, WinW, WinH]|GeomsTail]),

	SP is BWminusBorders - WinW * WinCnt,
	fill_spare_pixels_h(SP, 0, [[BX, BY, WinW, WinH]|GeomsTail], AdjustedGeoms),

	inner_gaps(GapPixel),
	(GapPixel = 0 ->
		Geoms = AdjustedGeoms
	;
		TotalSub is GapPixel * (WinCnt - 1),
		SizeSub is floor(TotalSub / WinCnt),
		layout:apply_inner_gaps_h(0, SizeSub, WinCnt, AdjustedGeoms, Geoms)
	)
.

calculate_layout(nrows(1), Mon, WinCnt, Bounds, Geoms) :- !, calculate_layout(hstack, Mon, WinCnt, Bounds, Geoms).
calculate_layout(nrows(N), Mon, WinCnt, Bounds, Geoms) :-
	(WinCnt =< N ->
		calculate_layout(stack, Mon, WinCnt, Bounds, Geoms)
	;
		WCDivN is WinCnt div N,
		WCModN is WinCnt mod N,
		calculate_layout(stack, Mon, N, Bounds, RowGs),
		findall(RGs,
			findall(Gs,
				(between(1, N, I), nth1(I, RowGs, RowBounds),
				(I =< WCModN -> CCnt is WCDivN + 1 ; CCnt is WCDivN),
				calculate_layout(hstack, Mon, CCnt, RowBounds, Gs)),
				RGs),
			AllGs),
		append(AllGs, L), append(L, Geoms))
.

calculate_layout(ncols(1), Mon, WinCnt, Bounds, Geoms) :- !, calculate_layout(stack, Mon, WinCnt, Bounds, Geoms).
calculate_layout(ncols(N), Mon, WinCnt, Bounds, Geoms) :-
	(WinCnt =< N ->
		calculate_layout(hstack, Mon, WinCnt, Bounds, Geoms)
	;
		WCDivN is WinCnt div N,
		WCModN is WinCnt mod N,
		calculate_layout(hstack, Mon, N, Bounds, ColGs),
		findall(CGs,
			findall(Gs,
				(between(1, N, I), nth1(I, ColGs, ColBounds),
				(I =< WCModN -> RCnt is WCDivN + 1 ; RCnt is WCDivN),
				calculate_layout(stack, Mon, RCnt, ColBounds, Gs)),
				CGs),
			AllGs),
		append(AllGs, L), append(L, Geoms))
.

calculate_layout(grid, Mon, WinCnt, Bounds, Geoms) :-
	Dimension is ceiling(sqrt(WinCnt)),
	calculate_layout(nrows(Dimension), Mon, WinCnt, Bounds, Geoms)

	% Note: one can play with rewriting the above ceiling to floor or nrows to ncols, to their liking,
	% getting another from the 4 slightly different grid layout sequences
.

calculate_layout(cmaster, Mon, WinCnt, [BX, BY, BW, BH], Geoms) :- !,
	global_key_value(active_ws, Mon, ActWs),
	global_key_value(nmaster, Mon-ActWs, Nmaster),
	global_key_value(mfact, Mon-ActWs, Mfact),
	StackWinCnt is max(0, WinCnt - Nmaster),
	RStackWinCnt is ceiling(StackWinCnt / 2),  % right stack will have 1 more win in odd cases
	LStackWinCnt is StackWinCnt - RStackWinCnt,
	% Only two peer stacks are needed
	(Nmaster == 0  ->
		LW is floor(BW / 2),
		inner_gaps(GapPixel),
		FinalLW is LW - floor(GapPixel / 2),
		RX is BX + LW + ceiling(GapPixel / 2),
		RW is BW - LW,
		calculate_layout(stack, Mon, LStackWinCnt, [BX, BY, FinalLW, BH], LGeoms),
		calculate_layout(stack, Mon, RStackWinCnt, [RX, BY, RW,      BH], RGeoms),
		CGeoms = []
	% All windows are in master area -> use a simple stack
	;(StackWinCnt == 0 ->
		calculate_layout(stack, Mon, WinCnt, [BX, BY, BW, BH], CGeoms),
		LGeoms = [], RGeoms = []
	% Single window to the right of the master(s)
	;(StackWinCnt == 1 ->
		MasterW is floor(BW * Mfact),
		StackW is BW - MasterW,
		StackX is BX + MasterW,
		inner_gaps(GapPixel),
		FinalMasterW is MasterW - floor(GapPixel / 2),
		FinalStackX  is StackX  + ceiling(GapPixel / 2),
		FinalStackW  is StackW  - ceiling(GapPixel / 2),
		MasterBounds = [BX,          BY, FinalMasterW, BH],
		StackBounds  = [FinalStackX, BY, FinalStackW,  BH],
		calculate_layout(stack, Mon, Nmaster,     MasterBounds, CGeoms),
		calculate_layout(stack, Mon, StackWinCnt, StackBounds,  RGeoms),
		LGeoms = []
	% Otherwise: all 3 stacks are needed
	;(
		MasterW is floor(BW * Mfact),
		StackW is floor((BW - MasterW) / 2),
		MasterX is BX + StackW,
		RStackX is MasterX + MasterW,
		inner_gaps(GapPixel),
		LStackW      is StackW - ceiling(GapPixel / 2),
		FinalMasterX is MasterX + floor(GapPixel / 2),
		FinalMasterW is MasterW - floor(GapPixel / 2) * 2,
		FinalRStackX is RStackX + ceiling(GapPixel / 2),
		FinalRStackW is StackW - ceiling(GapPixel / 2),
		LStackBounds = [BX,           BY, LStackW,      BH],
		MasterBounds = [FinalMasterX, BY, FinalMasterW, BH],
		RStackBounds = [FinalRStackX, BY, FinalRStackW, BH],
		calculate_layout(stack, Mon, LStackWinCnt, LStackBounds, LGeoms),
		calculate_layout(stack, Mon, Nmaster,      MasterBounds, CGeoms),
		calculate_layout(stack, Mon, RStackWinCnt, RStackBounds, RGeoms))))
	),
	utils:alternate_merge(RGeoms, LGeoms, RLGeoms),
	append(CGeoms, RLGeoms, Geoms)
.

calculate_layout(MasterType, Mon, WinCnt, [BX, BY, BW, BH], Geoms) :-
	member(MasterType, [lmaster, rmaster, tmaster, bmaster]),
	global_key_value(active_ws, Mon, ActWs),
	global_key_value(nmaster, Mon-ActWs, Nmaster),
	global_key_value(mfact, Mon-ActWs, Mfact),
	StackWinCnt is max(0, WinCnt - Nmaster),

	((Nmaster == 0 ; StackWinCnt == 0) ->  % only one stack is needed
		((MasterType = lmaster ; MasterType = rmaster) ->
			calculate_layout(stack, Mon, WinCnt, [BX, BY, BW, BH], Geoms)
		; % top, bottom
			calculate_layout(hstack, Mon, WinCnt, [BX, BY, BW, BH], Geoms))
	;
	((MasterType = lmaster ; MasterType = rmaster) ->
		MasterW is floor(BW * Mfact),
		StackW is BW - MasterW,
		(MasterType = lmaster ->
			inner_gaps(GapPixel),
			StackX       is BX + MasterW + ceiling(GapPixel / 2),
			FinalStackW  is StackW - ceiling(GapPixel / 2),
			FinalMasterW is MasterW - floor(GapPixel / 2),
			StackBounds  = [StackX, BY, FinalStackW,  BH],
			MasterBounds = [BX,     BY, FinalMasterW, BH]
		;MasterType = rmaster ->
			inner_gaps(GapPixel),
			MasterX      is BX + StackW + ceiling(GapPixel / 2),
			FinalMasterW is MasterW - ceiling(GapPixel / 2),
			FinalStackW  is StackW - floor(GapPixel / 2),
			MasterBounds = [MasterX, BY, FinalMasterW, BH],
			StackBounds  = [BX     , BY, FinalStackW , BH]),
		calculate_layout(stack, Mon, Nmaster,     MasterBounds, MasterGeoms),
		calculate_layout(stack, Mon, StackWinCnt, StackBounds,  StackGeoms)
	; % top, bottom
		(
		MasterH is floor(BH * Mfact),
		StackH is BH - MasterH,
		(MasterType = tmaster ->
			inner_gaps(GapPixel),
			StackY       is BY + MasterH + ceiling(GapPixel / 2),
			FinalStackH  is StackH - ceiling(GapPixel / 2),
			FinalMasterH is MasterH - floor(GapPixel / 2),
			MasterBounds = [BX, BY    , BW, FinalMasterH],
			StackBounds  = [BX, StackY, BW, FinalStackH]
		;MasterType = bmaster ->
			inner_gaps(GapPixel),
			MasterY     is BY + StackH + ceiling(GapPixel / 2),
			FinalMasterH is MasterH - ceiling(GapPixel / 2),
			FinalStackH  is StackH - floor(GapPixel / 2),
			MasterBounds = [BX, MasterY, BW, FinalMasterH],
			StackBounds  = [BX, BY     , BW, FinalStackH]),
		calculate_layout(hstack, Mon, Nmaster,     MasterBounds, MasterGeoms),
		calculate_layout(hstack, Mon, StackWinCnt, StackBounds,  StackGeoms)
		)
	),
	append(MasterGeoms, StackGeoms, Geoms)
	)
.


%*********************************  Helpers  **********************************

%! max_border_width(-MaxBorderW:integer) is det
%
%  Returns the largest between border_width/1 and border_width_focused/1.
%
%  @arg MaxBorderW the largest configured border width
max_border_width(MaxBorderW) :-
	user:border_width(BorderW),
	user:border_width_focused(BorderWF),
	MaxBorderW is max(BorderW, BorderWF)
.

%! stack_xs(+Geom:[integer], -NewGeom:[integer]) is det
%
%  Increments the x coordinate in a geometry by the width and two times the border width.
%
%  @arg Geom input geometry
%  @arg NewGeom output geometry with incremented x coordinate
stack_xs([X, Y, W, H], [NewX, Y, W, H]) :- max_border_width(BorderW), NewX is X + W + 2 * BorderW.

%! stack_ys(+Geom:[integer], -NewGeom:[integer]) is det
%
%  Increments the y coordinate in a geometry by the height and two times the border width.
%
%  @arg Geom input geometry
%  @arg NewGeom output geometry with incremented y coordinate
stack_ys([X, Y, W, H], [X, NewY, W, H]) :- max_border_width(BorderW), NewY is Y + H + 2 * BorderW.

%! fill_spare_pixels_v(++SparePixels:integer, ++Added:integer, ++Geoms:[[integer]], -NewGeoms:[[integer]]) is det
%
%  Vertically distribute spare pixels among window geometries.
%  The y coordinates and heights of geometries are incremented evenly.
%
%  @arg SparePixels count of pixels to distribute
%  @arg Added count of already distributed pixels
%  @arg Geoms input geometries
%  @arg NewGeoms output geometries
fill_spare_pixels_v(_, _, [], []).
fill_spare_pixels_v(0, Added, [[X, Y, W, H]|Gs], [[X, NewY, W, H]|NewGs]) :-
	!, NewY is Y + Added,
	fill_spare_pixels_v(0, Added, Gs, NewGs)
.
fill_spare_pixels_v(SP, Added, [[X, Y, W, H]|Gs], [[X, NewY, W, NewH]|NewGs]) :-
	0 < SP,
	SPminus1 is SP - 1, Addedplus1 is Added + 1,
	NewY is Y + Added, NewH is H + 1,
	fill_spare_pixels_v(SPminus1, Addedplus1, Gs, NewGs)
.

%! fill_spare_pixels_h(++SparePixels:integer, ++Added:integer, ++Geoms:[[integer]], -NewGeoms:[[integer]]) is det
%
%  Horizontally distribute spare pixels among window geometries.
%  The x coordinates and widths of geometries are incremented evenly.
%
%  @arg SparePixels count of pixels to distribute
%  @arg Added count of already distributed pixels, should be 0 at the top-level call
%  @arg Geoms input geometries
%  @arg NewGeoms output geometries
fill_spare_pixels_h(_, _, [], []).
fill_spare_pixels_h(0, Added, [[X, Y, W, H]|Gs], [[NewX, Y, W, H]|NewGs]) :-
	!, NewX is X + Added,
	fill_spare_pixels_h(0, Added, Gs, NewGs)
.
fill_spare_pixels_h(SP, Added, [[X, Y, W, H]|Gs], [[NewX, Y, NewW, H]|NewGs]) :-
	0 < SP,
	SPminus1 is SP - 1, Addedplus1 is Added + 1,
	NewX is X + Added, NewW is W + 1,
	fill_spare_pixels_h(SPminus1, Addedplus1, Gs, NewGs)
.

%! apply_inner_gaps_v(++I:integer, ++SizeSub:integer, ++WinCnt:integer, ++Geoms:[[integer]], -NewGeoms:[[integer]]) is det
%
%  Applies vertical inner gaps on window geometries, i.e. evenly subtracts from heights
%  and shifts y coordinates accordingly.
%
%  @arg I index of processed window, should be 0 at the top-level call
%  @arg SizeSub amount of pixels to subtract from each window's height
%  @arg WinCnt count of windows in the current layout
%  @arg Geoms input geometries
%  @arg NewGeoms output geometries with inner gaps applied
apply_inner_gaps_v(_, _, _, [], []).
apply_inner_gaps_v(I, SizeSub, WinCnt, [[X, Y, W, H]|Gs], [[X, NewY, W, NewH]|NewGs]) :-
	NewH is H - SizeSub,
	(I == 0 -> NewY is Y ; NewY is Y + floor(SizeSub / (WinCnt - I))),
	Iplus1 is I + 1,
	apply_inner_gaps_v(Iplus1, SizeSub, WinCnt, Gs, NewGs)
.

%! apply_inner_gaps_h(++I:integer, ++SizeSub:integer, ++WinCnt:integer, ++Geoms:[[integer]], -NewGeoms:[[integer]]) is det
%
%  Applies horizontal inner gaps on window geometries, i.e. evenly subtracts from widths
%  and shifts x coordinates accordingly.
%
%  @arg I index of processed window, should be 0 at the top-level call
%  @arg SizeSub amount of pixels to subtract from each window's width
%  @arg WinCnt count of windows in the current layout
%  @arg Geoms input geometries
%  @arg NewGeoms output geometries with inner gaps applied
apply_inner_gaps_h(_, _, _, [], []).
apply_inner_gaps_h(I, SizeSub, WinCnt, [[X, Y, W, H]|Gs], [[NewX, Y, NewW, H]|NewGs]) :-
	NewW is W - SizeSub,
	(I == 0 -> NewX is X ; NewX is X + floor(SizeSub / (WinCnt - I))),
	Iplus1 is I + 1,
	apply_inner_gaps_h(Iplus1, SizeSub, WinCnt, Gs, NewGs)
.

%! apply_geoms(++Wins:[integer], ++Geoms:[integer]) is det
%
%  Sets the geometries of the specified windows by moving and resizing them.
%
%  If animation_enabled/1 is true, then the move-resize calls will be interpolated
%  according to animation_time/1 and animation_granularity/1,
%  or by using default values if they are absent.
%
%  @arg Wins list of XIDs of managed windows
%  @arg Geoms list of geometries to set for Wins, must have the same length as Wins
apply_geoms(Wins, Geoms) :-
	utils:pair_up_lists(Wins, Geoms, WinGeomMap),

	(user:animation_enabled(true) ->
		animation_time(AnimT), animation_granularity(AnimG),
		findall(ThreadId, (
			member(Win-[NewX, NewY, NewW, NewH], WinGeomMap),
			user:win_properties(Win, [_, _, [X, Y, W, H]]),
			thread_create(
				animation:interpolate_geom(Win, X, Y, W, H, NewX, NewY, NewW, NewH, AnimG, AnimT)
			, ThreadId),
			user:win_newproperties(Win, [managed, false, [NewX, NewY, NewW, NewH]])
		), ThreadIds),
		compat_forall(member(ThreadId, ThreadIds), thread_join(ThreadId))
	;
		compat_forall(member(Win-[NewX, NewY, NewW, NewH], WinGeomMap), (
			user:display(Dp),
			plx:x_move_resize_window(Dp, Win, NewX, NewY, NewW, NewH),
			user:win_newproperties(Win, [managed, false, [NewX, NewY, NewW, NewH]])
		))
	)
.
