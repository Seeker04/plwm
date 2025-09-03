% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

% This module contains settings related operations:
%   setting/1      defines atoms which plwm considers settings
%   init_config/0  validates user config and assigns default values to missing/invalid settings
%   valid_set/2    checks if a value is valid for a setting
%   default_set/2  relates default values to settings
%   set/2          modifies settings
%   add/2          appends to list typed settings

:- module(setting, [set/2, add/2]).

:- use_module(utils).

%! setting(+Setting:atom) is semidet
%  setting(-Setting:atom) is nondet
%
%  Tests if an atom is a setting.
%  Can also be used to generate the list of settings.
%
%  @arg Setting atom that is a setting
setting(Setting) :- member(Setting, [
	default_nmaster, default_mfact, default_layout, attach_bottom,
	border_width, border_width_focused, border_color, border_color_focused,
	snap_threshold, outer_gaps, inner_gaps, workspaces, starting_workspace,
	hide_empty_workspaces, ws_format, ws_format_occupied, layout_default_overrides,
	bar_classes, bar_placement, fifo_enabled, fifo_path, menucmd, animation_enabled,
	animation_time, animation_granularity, modkey, scroll_up_action, scroll_down_action,
	keymaps, rules, hooks
]).

%! init_config(++Dryrun:bool) is det
%
%  This predicate does two things:
%    1. Validates all settings that were specified in the config file (errors are printed)
%    2. Assigns the default value to all unset and invalid settings
%
%  @arg Dryrun if true, only the validation is executed
init_config(Dryrun) :-
	forall(setting(Setting), (
		(call(user:Setting, Value) ->
			(\+ valid_set(Setting, Value) ->
				(Dryrun = false ->
					default_set(Setting, DefaultValue),
					store_setting(Setting, DefaultValue)
				; true)
			; true)
		;
			(Dryrun = false ->
				default_set(Setting, DefaultValue),
				store_setting(Setting, DefaultValue)
			; true)
		)
	)),

	% Check some cross-setting validity, e.g. that starting_workspace/1 is contained in workspaces/1
	(user:starting_workspace(StartingWs), user:workspaces([WssHead|WssTail]), \+ member(StartingWs, [WssHead|WssTail]) ->
	compat_format(string(Msg), "warning: starting_workspace: ~a is not a workspace, defaulting to ~a",
		[StartingWs, WssHead]),
		writeln(user_error, Msg),
		(Dryrun = false ->
			store_setting(starting_workspace, WssHead)
		; true)
	; true)
.

%! valid_set(++Setting:atom, ++Value:term) is semidet
%
%  Checks if the given value is valid for the specified setting.
%  Fails if the setting does not exist or the value is invalid.
%
%  @arg Setting setting to check new value for
%  @arg Value new value candidate to validate
valid_set(Setting, Value) :-
	(\+ valid_set_(Setting, Value) ->
		warn_invalid_setting(Setting, Value),
		fail
	; true)
.

valid_set_(default_nmaster,          Value) :- integer(Value), 0 =< Value.
valid_set_(default_mfact,            Value) :- utils:is_float(Value), 0.05 =< Value, Value =< 0.95.
valid_set_(default_layout,           Value) :- layout:is_layout(Value).
valid_set_(attach_bottom,            Value) :- Value = true ; Value = false.
valid_set_(border_width,             Value) :- integer(Value), 0 =< Value.
valid_set_(border_width_focused,     Value) :- integer(Value), 0 =< Value.
valid_set_(border_color,             Value) :- string(Value).
valid_set_(border_color_focused,     Value) :- string(Value).
valid_set_(snap_threshold,           Value) :- integer(Value), 0 =< Value.
valid_set_(outer_gaps,               Value) :- integer(Value), 0 =< Value.
valid_set_(inner_gaps,               Value) :- integer(Value), 0 =< Value.
valid_set_(workspaces,               Value) :- Value \= [], is_set(Value), forall(member(Ws, Value), atom(Ws)).
valid_set_(starting_workspace,       Value) :- atom(Value).
valid_set_(hide_empty_workspaces,    Value) :- Value = true ; Value = false.
valid_set_(ws_format,                Value) :- catch(user:format_ws_name(Value, [1, a], _), _, fail).
valid_set_(ws_format_occupied,       Value) :- catch(user:format_ws_name(Value, [1, a], _), _, fail).
valid_set_(bar_classes,              Value) :- is_list(Value), forall(member(Pair, Value),
                                               (Pair = N-C, (string(N) ; var(N)), (string(C) ; var(C)))).
valid_set_(bar_placement,            Value) :- Value = follow_focus ; Value = static.
valid_set_(fifo_enabled,             Value) :- Value = true ; Value = false.
valid_set_(fifo_path,                Value) :- string(Value).
valid_set_(menucmd,                  Value) :- is_list(Value), forall(member(Arg, Value), string(Arg)).
valid_set_(animation_enabled,        Value) :- Value = true ; Value = false.
valid_set_(animation_time,           Value) :- utils:is_float(Value), 0.0 < Value.
valid_set_(animation_granularity,    Value) :- integer(Value), 1 =< Value.
valid_set_(modkey,                   Value) :- user:modifier(Value).
valid_set_(scroll_up_action,         Value) :- utils:valid_callable(Value) ; Value = none.
valid_set_(scroll_down_action,       Value) :- utils:valid_callable(Value) ; Value = none.
valid_set_(layout_default_overrides, Value) :-
	is_list(Value),
	forall(member(Override, Value), (
		Override = (MonOR, WsOR -> NmasterOR, MfactOR, LayoutOR),
		(var(MonOR)     ; string(MonOR)),
		(var(WsOR)      ; atom(WsOR)),
		(var(NmasterOR) ; integer(NmasterOR), 0 =< NmasterOR),
		(var(MfactOR)   ; utils:is_float(MfactOR), 0.05 =< MfactOR, MfactOR =< 0.95),
		(var(LayoutOR)  ; layout:is_layout(LayoutOR))
	))
.
valid_set_(rules, Value) :-
	is_list(Value),
	forall(member(Rule, Value), (
		Rule = (RName, RClass, RTitle -> RMon, RWs, RMode),
		(var(RName)  ; string(RName)  ; (RName  = exact(Str), string(Str))),
		(var(RClass) ; string(RClass) ; (RClass = exact(Str), string(Str))),
		(var(RTitle) ; string(RTitle) ; (RTitle = exact(Str), string(Str))),
		(var(RMon)   ; string(RMon)),
		(var(RWs)    ; atom(RWs) ; (integer(RWs), 0 < RWs)),
		(var(RMode)  ; RMode = managed ; RMode = floating ; RMode = fullscreen
			     ; (is_list(RMode), apply(geometry_spec, RMode)))
	))
.
valid_set_(hooks, Value) :-
	is_list(Value),
	forall(member(Hook, Value), (
		Hook = (Event -> Action),
		member(Event, [
			start, quit, switch_workspace_pre, switch_workspace_post,
			switch_monitor_pre, switch_monitor_post, window_create_pre,
			window_create_post, window_destroy_pre, window_destroy_post]),
		utils:valid_callable(Action)
	))
.
valid_set_(keymaps, Value) :-
	is_list(Value),
	forall(member(Keymap, Value), (
		Keymap = (Keybind -> Action),
		user:keybind_to_keylist(Keybind, KeyList),
		forall(member(Key, KeyList), (user:modifier(Key) ; last(KeyList, Key))),
		(utils:valid_callable(Action) ; Action = none)
	))
.

%! default_set(++Setting:atom, -Value:term) is semidet
%
%  Queries default values of settings.
%  Fails for invalid setting names.
%
%  @arg Setting setting to query the default value for
%  @arg Value default value of the setting
default_set(default_nmaster,          1).
default_set(default_mfact,            2/3).
default_set(default_layout,           lmaster).
default_set(attach_bottom,            false).
default_set(border_width,             1).
default_set(border_width_focused,     1).
default_set(border_color,             "gray").
default_set(border_color_focused,     "blue").
default_set(snap_threshold,           32).
default_set(outer_gaps,               0).
default_set(inner_gaps,               0).
default_set(workspaces,               ['1', '2', '3', '4', '5', '6', '7', '8', '9']).
default_set(starting_workspace,       '1').
default_set(hide_empty_workspaces,    false).
default_set(ws_format,                "~w").
default_set(ws_format_occupied,       "▘~w").
default_set(layout_default_overrides, []).
default_set(bar_classes,              ["polybar"-"Polybar"]).
default_set(bar_placement,            follow_focus).
default_set(fifo_enabled,             false).
default_set(fifo_path,                "/tmp/plwm_fifo").
default_set(menucmd,                  ["dmenu", "-i", "-l", "20", "-p"]).
default_set(animation_enabled,        false).
default_set(animation_time,           0.2).
default_set(animation_granularity,    30).
default_set(modkey,                   super).
default_set(scroll_up_action,         switch_workspace(next)).
default_set(scroll_down_action,       switch_workspace(prev)).
default_set(rules,                    []).
default_set(hooks,                    [(start -> writeln("plwm starting")), (quit -> writeln("plwm quitting"))]).
default_set(keymaps, [
  (super +         j         ->  shift_focus(down))              ,
  (super +         k         ->  shift_focus(up))                ,
  (super + shift + k         ->  move_focused(up))                ,
  (super + shift + j         ->  move_focused(down))              ,
  (super +         "Return"  ->  focused_to_top)                  ,
  (super +         q         ->  close_focused)                   ,
  (super + shift + space     ->  toggle_floating)                 ,
  (super +         f         ->  toggle_fullscreen)               ,
  (super + shift + q         ->  quit)                            ,
  (super + i                 ->  change_nmaster(+1))              ,
  (super + d                 ->  change_nmaster(-1))              ,
  (super + h                 ->  change_mfact(-0.05))             ,
  (super + l                 ->  change_mfact(+0.05))             ,
  (super + shift + f         ->  layout:set_layout(floating))     ,
  (super + shift + m         ->  layout:set_layout(monocle))      ,
  (super + shift + s         ->  layout:set_layout(stack))        ,
  (super + shift + h         ->  layout:set_layout(hstack))       ,
  (super + shift + g         ->  layout:set_layout(grid))         ,
  (super + shift + l         ->  layout:set_layout(lmaster))      ,
  (super + shift + r         ->  layout:set_layout(rmaster))      ,
  (super + shift + t         ->  layout:set_layout(tmaster))      ,
  (super + shift + b         ->  layout:set_layout(bmaster))      ,
  (super + shift + c         ->  layout:set_layout(cmaster))      ,
  (super +         "Tab"     ->  toggle_workspace)                ,
  (super + shift + "Tab"     ->  toggle_hide_empty_workspaces)    ,
  (super + 1                 ->  switch_workspace('1'))           ,
  (super + 2                 ->  switch_workspace('2'))           ,
  (super + 3                 ->  switch_workspace('3'))           ,
  (super + 4                 ->  switch_workspace('4'))           ,
  (super + 5                 ->  switch_workspace('5'))           ,
  (super + 6                 ->  switch_workspace('6'))           ,
  (super + 7                 ->  switch_workspace('7'))           ,
  (super + 8                 ->  switch_workspace('8'))           ,
  (super + 9                 ->  switch_workspace('9'))           ,
  (super + p                 ->  switch_workspace(prev))          ,
  (super + n                 ->  switch_workspace(next))          ,
  (super + shift + 1         ->  move_focused_to_workspace('1'))  ,
  (super + shift + 2         ->  move_focused_to_workspace('2'))  ,
  (super + shift + 3         ->  move_focused_to_workspace('3'))  ,
  (super + shift + 4         ->  move_focused_to_workspace('4'))  ,
  (super + shift + 5         ->  move_focused_to_workspace('5'))  ,
  (super + shift + 6         ->  move_focused_to_workspace('6'))  ,
  (super + shift + 7         ->  move_focused_to_workspace('7'))  ,
  (super + shift + 8         ->  move_focused_to_workspace('8'))  ,
  (super + shift + 9         ->  move_focused_to_workspace('9'))  ,
  (super + shift + p         ->  move_focused_to_workspace(prev)) ,
  (super + shift + n         ->  move_focused_to_workspace(next)) ,
  (super +         comma     ->  switch_monitor(prev))            ,
  (super +         period    ->  switch_monitor(next))            ,
  (super + shift + comma     ->  move_focused_to_monitor(prev))   ,
  (super + shift + period    ->  move_focused_to_monitor(next))   ,
  (alt +         w           ->  menu:goto_window)                ,
  (alt + shift + w           ->  menu:goto_workspace)             ,
  (alt +         p           ->  menu:pull_from)                  ,
  (alt + shift + p           ->  menu:push_to)                    ,
  (alt + q                   ->  menu:close_windows)              ,
  (alt + shift + q           ->  menu:keep_windows)               ,
  (alt + c                   ->  menu:create_workspace)           ,
  (alt + r                   ->  menu:rename_workspace)           ,
  (alt + i                   ->  menu:reindex_workspace)          ,
  (alt + d                   ->  menu:delete_workspaces)          ,
  (alt + shift + k           ->  menu:list_keymaps)               ,
  (alt + shift + c           ->  menu:list_cmds)                  ,
  (ctrl + shift + space      ->  shellcmd("alacritty"))           ,
  (alt + a                   ->  shellcmd("dmenu_run -l 20 -p run"))
]).

%! set(++Setting:atom, ++Value:term) is semidet
%
%  Assigns a configuration setting a new value.
%
%  If either the setting or the value is invalid, a warning will be printed and
%  the predicate fails.
%
%  For each setting (which requires it), additional logic will be run in order
%  to make the change immediately visible to the user
%  (e.g. changing gap sizes will trigger a relayout).
%
%  @arg Setting configuration to set
%  @arg Value new value for the configuration
set(Setting, Value) :-
	valid_set(Setting, Value),
	set_(Setting, Value)
.

set_(default_nmaster,          Value) :- store_setting(default_nmaster, Value).
set_(default_mfact,            Value) :- store_setting(default_mfact, Value).
set_(default_layout,           Value) :- store_setting(default_layout, Value).
set_(attach_bottom,            Value) :- store_setting(attach_bottom, Value).
set_(border_width,             Value) :- store_setting(border_width, Value),         update_all_borders.
set_(border_width_focused,     Value) :- store_setting(border_width_focused, Value), update_all_borders.
set_(border_color,             Value) :- store_setting(border_color, Value),         alloc_colors, update_all_borders.
set_(border_color_focused,     Value) :- store_setting(border_color_focused, Value), alloc_colors, update_all_borders.
set_(snap_threshold,           Value) :- store_setting(snap_threshold, Value).
set_(outer_gaps,               Value) :- store_setting(outer_gaps, Value), update_free_win_space.
set_(inner_gaps,               Value) :- store_setting(inner_gaps, Value), layout:relayout.
set_(workspaces,               Value) :- store_setting(workspaces, Value), set_workspaces.
set_(starting_workspace,       Value) :- store_setting(starting_workspace, Value). % no sense to change, but we allow it
set_(hide_empty_workspaces,    Value) :- store_setting(hide_empty_workspaces, Value), update_ws_atoms.
set_(ws_format,                Value) :- store_setting(ws_format, Value),             update_ws_atoms.
set_(ws_format_occupied,       Value) :- store_setting(ws_format_occupied, Value),    update_ws_atoms.
set_(layout_default_overrides, Value) :- store_setting(layout_default_overrides, Value).
set_(bar_classes,              Value) :- store_setting(bar_classes, Value).
set_(bar_placement,            Value) :- store_setting(bar_placement, Value), update_free_win_space.
set_(fifo_enabled,             Value) :- store_setting(fifo_enabled, Value). % TODO: handle this
set_(fifo_path,                Value) :- store_setting(fifo_path, Value). % TODO: handle this
set_(menucmd,                  Value) :- store_setting(menucmd, Value).
set_(animation_enabled,        Value) :- store_setting(animation_enabled, Value).
set_(animation_time,           Value) :- store_setting(animation_time, Value).
set_(animation_granularity,    Value) :- store_setting(animation_granularity, Value).
set_(modkey,                   Value) :- store_setting(modkey, Value), grab_buttons.
set_(scroll_up_action,         Value) :- store_setting(scroll_up_action, Value).
set_(scroll_down_action,       Value) :- store_setting(scroll_down_action, Value).
set_(keymaps,                  Value) :- store_setting(keymaps, Value), grab_keys.
set_(rules,                    Value) :- store_setting(rules, Value).
set_(hooks,                    Value) :- store_setting(hooks, Value), setup_hooks.

%! add(++Setting:atom, ++Value:term) is semidet
%
%  Works exactly like set/2, but instead of overwriting, it appends to list typed settings.
%  It is only defined for list typed settings.
%
%  @arg Setting configuration to set
%  @arg Value new value to append to the configuration
add(Setting, Value) :-
	member(Setting, [workspaces, layout_default_overrides, menucmd, keymaps, rules, hooks]),
	call(Setting, PrevList),
	append(PrevList, [Value], NewList),
	valid_set(Setting, NewList),
	set_(Setting, NewList)
.

%*********************************  Helpers  **********************************

%! warn_invalid_setting(++Setting:atom, ++Value:term) is det
%
%  Prints a warning to stderr about an invalid setting value.
%
%  @arg Setting setting that got invalid value
%  @arg Value invalid value
warn_invalid_setting(Setting, Value) :-
compat_format(string(Msg), "warning: invalid value: ~w for setting: ~a, ignored", [Value, Setting]),
	writeln(user_error, Msg)
.

%! store_setting(++Setting:atom, ++Value:term) is det
%
%  Stores a new setting value in a dynamic predicate (retracting any previous value first).
%
%  Warning: this predicate is for internal use! It does not check if the setting is
%  a proper configuration or if the new value is valid. Users should use set/2 instead.
%
%  @arg Setting setting to store
%  @arg Value new value for the setting
store_setting(Setting, Value) :-
	compound_name_arguments(Config, Setting, [Value]),
	reassert(user:Config)
.

%! geometry_spec(++X:integer, ++Y:integer, ++W:integer, ++H:integer) is semidet
%
%  Checks whether X,Y,W,H form a window geometry specification, fails if not.
%
%  @arg X horizontal coordinate, percentage from left screen edge or left, right, center
%  @arg Y vertical coordinate, percentage from top screen edge or top, bottom, center
%  @arg W width in pixels or percentage of screen width
%  @arg H height in pixels or percentage of screen height
geometry_spec(X, Y, W, H) :-
	(var(X) ; (integer(X), 0 =< X) ; (utils:is_float(X), 0 =< X, X =< 1) ; member(X, [left, right, center])),
	(var(Y) ; (integer(Y), 0 =< Y) ; (utils:is_float(Y), 0 =< Y, Y =< 1) ; member(Y, [top, bottom, center])),
	(var(W) ; (integer(W), 0  < W) ; (utils:is_float(W), 0 =< W, W =< 1)),
	(var(H) ; (integer(H), 0  < H) ; (utils:is_float(H), 0 =< H, H =< 1))
.

%! update_all_borders() is det
%
%  Applies border_width/1 and border_width_focused/1 on all windows.
%  Also redraws layout to make changes immediately visible.
update_all_borders :-
	monws_keys(Keys),
	forall(member(Mon-Ws, Keys), (
		global_key_value(windows, Mon-Ws, Wins),
		forall(member(Win, Wins), (
			set_border(Win)
		))
	)),
	layout:relayout
.

%! set_workspaces() is det
%
%  Re-applies the workspaces/1 setting.
%  It deletes all existing workspace that is not present in the new workspaces/1 setting,
%  then it creates the new workspaces.
%
%  Note: workspaces present in both the old and new workspaces/1 will keep their windows,
%        otherwise the usual semantics will "shift windows to the right",
%        [a, b, c] -> [x, y, z] will result in all windows being on x.
set_workspaces :-
	user:workspaces(NewWss), nb_getval(workspaces, OldWss),

	subtract(OldWss, NewWss, ToDelete), % delete workspaces no longer in workspaces/1
	forall(member(Ws, ToDelete), delete_workspace(Ws)),

	forall(member(Ws, NewWss), create_workspace(Ws)),

	(OldWss = ToDelete ->               % cleanup last survivor if all old wss were to be deleted
		last(ToDelete, SurvivorWs), % (because we refuse to remove the final ws if only 1 is left)
		delete_workspace(SurvivorWs)
	; true)
.
