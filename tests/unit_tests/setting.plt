% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

% mocks (and some essential definitions)
is_float(X) :- float(X), ! ; (X = N/D, integer(N), integer(D)).
is_layout(L) :- member(L, [floating, monocle, stack, hstack, nrows(N), ncols(N),
                           grid, lmaster, rmaster, tmaster, bmaster, cmaster]).
format_ws_name(Fmt, [Idx, Ws], Formatted) :-
	(sub_string(Fmt, _, _, _, "~d") -> format(atom(Formatted), Fmt, [Idx, Ws]) ;
	                                   format(atom(Formatted), Fmt, [Ws]))
.
modifier(Mod) :- member(Mod, [shift, lock, ctrl, alt, mod2, mod3, super, mod5]).
valid_callable(Pred) :- callable(Pred).
keybind_to_keylist(A, [A]) :- atom(A) ; string(A) ; integer(A).
keybind_to_keylist(L + R, List) :-
	keybind_to_keylist(L, LL),
	keybind_to_keylist(R, RL),
	append(LL, RL, List)
.
n_item_clones(0, _, []) :- !.
n_item_clones(N, T, [T|Ts]) :-
	N > 0,
	Nminus1 is N - 1,
	n_item_clones(Nminus1, T, Ts)
.
reassert(Pred) :-
	functor(Pred, Name, Arity),
	n_item_clones(Arity, _, Placeholders),
	compound_name_arguments(PredToRetract, Name, Placeholders),
	retractall(PredToRetract),
	assertz(Pred)
.
monws_keys(["Mon1"-ws1, "Mon1"-ws2]).
global_key_value(windows, "Mon1"-ws1, [1, 2]).
global_key_value(windows, "Mon1"-ws2, [3, 4, 5]).
delete_workspace(Ws) :- nb_getval(workspaces, Wss), member(Ws, Wss).
create_workspace(Ws) :- atom(Ws).
set_border(N) :- integer(N).
switch_workspace(next). switch_workspace(prev).
alloc_colors. update_free_win_space. layout:relayout. set_workspaces. update_ws_atoms.
grab_buttons. grab_keys. setup_hooks.

:- begin_tests(setting_tests).

:- use_module("../../src/setting").

test("setting + (elements)") :-
	assertion(findall(Setting, setting:setting(Setting), [
	default_nmaster, default_mfact, default_layout, attach_bottom,
	border_width, border_width_focused, border_color, border_color_focused,
	snap_threshold, outer_gaps, inner_gaps, workspaces, starting_workspace,
	hide_empty_workspaces, ws_format, ws_format_occupied, layout_default_overrides,
	bar_classes, bar_placement, fifo_enabled, fifo_path, menucmd, animation_enabled,
	animation_time, animation_granularity, modkey, scroll_up_action, scroll_down_action,
	keymaps, rules, hooks]))
.

test("setting + (all atoms)") :-
	assertion(forall(setting:setting(Setting), atom(Setting)))
.

test("setting - (no compound)") :-
	assertion(\+ (setting:setting(Setting), compound(Setting)))
.

test("valid_set + (defaults are valid)") :-
	assertion(forall(setting:setting(Setting), (
		setting:default_set(Setting, DefValue),
		setting:valid_set(Setting, DefValue)
	)))
.

test("valid_set - (non-existent setting)") :-
	assertion(\+ setting:valid_set(foo, _))
.

test("valid_set - (incorrect value)") :-
	assertion(\+ setting:valid_set(default_nmaster, foo))
.

test("default_set + (all settings covered, nothing else)") :-
	assertion(findall(Setting, setting:setting(Setting), Settings)),
	assertion(findall(Setting, setting:default_set(Setting, _), Settings))
.

test("default_set - (non-existent setting)") :-
	assertion(\+ setting:default_set(foo, _))
.

test("set + (defaults can be set)", [
	setup(
		nb_setval(workspaces, [ws1, ws2])
	),
	cleanup((
		nb_delete(workspaces),
		forall(setting:setting(Setting), (
			compound_name_arguments(Config, Setting, [_]),
			retractall(Config)
		))
	))
]) :-
	% set succeeds for all defaults
	assertion(forall(setting:default_set(Setting, DefValue), set(Setting, DefValue))),

	% they were indeed set and nothing else was
	assertion(forall(setting:setting(Setting), (
		setting:default_set(Setting, DefValue),
		findall(Value, call(Setting, Value), [DefValue])
	)))
.

test("set + (overwriting, determinism kept)", [
	cleanup(
		retractall(default_nmaster(_))
	)
]) :-
	assertion(set(default_nmaster, 1)),
	assertion(findall(N, setting:default_nmaster(N), [1])),
	assertion(set(default_nmaster, 2)),
	assertion(findall(N, setting:default_nmaster(N), [2])),
	assertion(set(default_nmaster, 3)),
	assertion(findall(N, setting:default_nmaster(N), [3]))
.

test("set - (non-existent setting)") :-
	assertion(\+ setting:set(foo, _))
.

test("set - (incorrect value)") :-
	assertion(\+ setting:set(default_nmaster, foo))
.

test("add +", [
	setup(
		nb_setval(workspaces, [ws1, ws2])
	),
	cleanup((
		nb_delete(workspaces),
		forall(setting:setting(Setting), (
			compound_name_arguments(Config, Setting, [_]),
			retractall(Config)
		))
	))
]) :-
	% workspaces
	assertion(set(workspaces, [a])),
	assertion(add(workspaces, b)),
	assertion(setting:workspaces([a, b])), % tests are modules implicitly, hence the prefix
	assertion(add(workspaces, c)),
	assertion(setting:workspaces([a, b, c])),

	% layout_default_overrides
	assertion(set(layout_default_overrides, [(_, a -> 3, 0.5, grid)])),
	assertion(add(layout_default_overrides, ("Mon1", _ -> 2, 0.7, rmaster))),
	assertion(setting:layout_default_overrides([
		(_     , a -> 3, 0.5, grid   ),
		("Mon1", _ -> 2, 0.7, rmaster)
	])),
	assertion(add(layout_default_overrides, (_, _ -> _, _, _))),
	assertion(setting:layout_default_overrides([
		(_     , a -> 3, 0.5, grid   ),
		("Mon1", _ -> 2, 0.7, rmaster),
		(_     , _ -> _, _  , _      )
	])),

	% menucmd
	assertion(set(menucmd, ["dmenu"])),
	assertion(add(menucmd, "-i")),
	assertion(setting:menucmd(["dmenu", "-i"])),
	assertion(add(menucmd, "-l")),
	assertion(setting:menucmd(["dmenu", "-i", "-l"])),

	% keymaps
	assertion(set(keymaps, [super + q -> close_focused])),
	assertion(add(keymaps, (super + shift + q -> quit))),
	assertion(setting:keymaps([
		super +         q -> close_focused,
		super + shift + q -> quit
	])),
	assertion(add(keymaps, ("AudioRaiseVolume" -> shellcmd("pulseaudio-ctl up")))),
	assertion(setting:keymaps([
		super +         q  -> close_focused,
		super + shift + q  -> quit,
		"AudioRaiseVolume" -> shellcmd("pulseaudio-ctl up")
	])),

	% rules
	assertion(set(rules, [("a", "b", "c" -> "Mon1", ws1, fullscreen)])),
	assertion(add(rules, (_, _, exact("c") -> _, ws1, [left, top, 1/2, 1/3]))),
	assertion(setting:rules([
		("a", "b", "c"        -> "Mon1", ws1, fullscreen           ),
		(_  , _  , exact("c") -> _     , ws1, [left, top, 1/2, 1/3])
	])),
	assertion(add(rules, (_, _, _ -> "Mon1", _, managed))),
	assertion(setting:rules([
		("a", "b", "c"        -> "Mon1", ws1, fullscreen           ),
		(_  , _  , exact("c") -> _     , ws1, [left, top, 1/2, 1/3]),
		(_  , _  , _          -> "Mon1", _  , managed              )
	])),

	% hooks
	assertion(set(hooks, [start -> writeln("plwm starting")])),
	assertion(add(hooks, quit -> writeln("plwm quitting"))),
	assertion(setting:hooks([
		start -> writeln("plwm starting"),
		quit  -> writeln("plwm quitting")
	])),
	assertion(add(hooks, window_create_post -> writeln("ws created"))),
	assertion(setting:hooks([
		start              -> writeln("plwm starting"),
		quit               -> writeln("plwm quitting"),
		window_create_post -> writeln("ws created")
	]))
.

test("add - (non-existent setting)") :-
	assertion(\+ setting:add(foo, _))
.

test("add - (non-list settings)") :-
	assertion(forall(setting:setting(Setting), (
		(\+ member(Setting, [workspaces, layout_default_overrides, menucmd, keymaps, rules, hooks])) ->
			\+ setting:add(Setting, _)
		; true)
	))
.

test("add - (invalid values)") :-
	assertion(\+ setting:add(workspaces, 1.0)),
	assertion(\+ setting:add(layout_default_overrides, foo)),
	assertion(\+ setting:add(menucmd, 42)),
	assertion(\+ setting:add(keymaps, foo + bar + z -> writeln("foo"))),
	assertion(\+ setting:add(rules, "bar")),
	assertion(\+ setting:add(hooks, baz))
.

test("warn_invalid_setting +") :-
	assertion(forall(setting:setting(Setting), (
		assertion(setting:warn_invalid_setting(Setting, foo))
	)))
.

test("warn_invalid_setting - (var setting)", [error(format_argument_type(_, _), _)]) :-
	setting:warn_invalid_setting(_, foo)
.

test("store_setting + (defaults can be stored)", [
	cleanup(
		forall(setting:setting(Setting), (
			compound_name_arguments(Config, Setting, [_]),
			retractall(Config)
		))
	)
]) :-
	% store_setting succeeds for all defaults
	assertion(forall(setting:default_set(Setting, DefValue), setting:store_setting(Setting, DefValue))),

	% they were indeed stored and nothing else was
	assertion(forall(setting:setting(Setting), (
		setting:default_set(Setting, DefValue),
		findall(Value, call(setting:Setting, Value), [DefValue])
	)))
.

test("store_setting + (overwriting, determinism kept)", [
	cleanup(
		retractall(default_nmaster(_))
	)
]) :-
	assertion(setting:store_setting(default_nmaster, 1)),
	assertion(findall(N, setting:default_nmaster(N), [1])),
	assertion(setting:store_setting(default_nmaster, 2)),
	assertion(findall(N, setting:default_nmaster(N), [2])),
	assertion(setting:store_setting(default_nmaster, 3)),
	assertion(findall(N, setting:default_nmaster(N), [3]))
.

test("store_setting - (var setting)", [error(instantiation_error, _)]) :-
	setting:store_setting(_, foo)
.

test("geometry_spec +") :-
	assertion(setting:geometry_spec(_     , _     , _  , _  )), % all var
	assertion(setting:geometry_spec(0     , 0     , 1  , 1  )), % min coord & size
	assertion(setting:geometry_spec(0.0   , 0.0   , 0.0, 0.0)), % min percent
	assertion(setting:geometry_spec(1.0   , 1.0   , 1.0, 1.0)), % max percent
	assertion(setting:geometry_spec(left  , _     , _  , _  )), % tokens
	assertion(setting:geometry_spec(right , _     , _  , _  )),
	assertion(setting:geometry_spec(center, _     , _  , _  )),
	assertion(setting:geometry_spec(_     , top   , _  , _  )),
	assertion(setting:geometry_spec(_     , bottom, _  , _  )),
	assertion(setting:geometry_spec(_     , center, _  , _  )),
	assertion(setting:geometry_spec(left  , top   , 0.0, 1.0)), % misc combinations
	assertion(setting:geometry_spec(center, center, _  , 1.0)),
	assertion(setting:geometry_spec(right , bottom, 1.0, _  ))
.

test("geometry_spec -") :-
	assertion(\+ setting:geometry_spec(-1    , _     , _     , _     )), % <min X
	assertion(\+ setting:geometry_spec(_     , -1    , _     , _     )), % <min Y
	assertion(\+ setting:geometry_spec(_     , _     , -1    , _     )), % <min W
	assertion(\+ setting:geometry_spec(_     , _     , _     , -1    )), % <min H
	assertion(\+ setting:geometry_spec(-0.01 , _     , _     , _     )), % <min X percent
	assertion(\+ setting:geometry_spec(_     , -0.01 , _     , _     )), % <min Y percent
	assertion(\+ setting:geometry_spec(_     , _     , -0.01 , _     )), % <min W percent
	assertion(\+ setting:geometry_spec(_     , _     , _     , -0.01 )), % <min H percent
	assertion(\+ setting:geometry_spec(1.01  , _     , _     , _     )), % >max X percent
	assertion(\+ setting:geometry_spec(_     , 1.01  , _     , _     )), % >max Y percent
	assertion(\+ setting:geometry_spec(_     , _     , 1.01  , _     )), % >max W percent
	assertion(\+ setting:geometry_spec(_     , _     , _     , 1.01  )), % >max H percent
	assertion(\+ setting:geometry_spec(foo   , _     , _     , _     )), % invalid tokens
	assertion(\+ setting:geometry_spec(_     , foo   , _     , _     )),
	assertion(\+ setting:geometry_spec(_     , _     , foo   , _     )),
	assertion(\+ setting:geometry_spec(_     , _     , _     , foo   )),
	assertion(\+ setting:geometry_spec(top   , _     , _     , _     )), % misplaced tokens
	assertion(\+ setting:geometry_spec(bottom, _     , _     , _     )),
	assertion(\+ setting:geometry_spec(_     , left  , _     , _     )),
	assertion(\+ setting:geometry_spec(_     , right , _     , _     )),
	assertion(\+ setting:geometry_spec(_     , _     , left  , _     )),
	assertion(\+ setting:geometry_spec(_     , _     , right , _     )),
	assertion(\+ setting:geometry_spec(_     , _     , top   , _     )),
	assertion(\+ setting:geometry_spec(_     , _     , bottom, _     )),
	assertion(\+ setting:geometry_spec(_     , _     , center, _     )),
	assertion(\+ setting:geometry_spec(_     , _     , _     , left  )),
	assertion(\+ setting:geometry_spec(_     , _     , _     , right )),
	assertion(\+ setting:geometry_spec(_     , _     , _     , top   )),
	assertion(\+ setting:geometry_spec(_     , _     , _     , bottom)),
	assertion(\+ setting:geometry_spec(_     , _     , _     , center))
.

test("update_all_borders", [
	setup((
		set(border_width, 1),
		set(border_width_focused, 2),
		set(border_color, "black"),
		set(border_color_focused, "red")
	)),
	cleanup((
		retractall(border_width(_)),
		retractall(border_width_focused(_)),
		retractall(border_color(_)),
		retractall(border_color_focused(_))
	))
]) :-
	assertion(setting:update_all_borders) % note: we mock set_border as a fact
.

test("set_workspaces", [
	setup(
		nb_setval(workspaces, [ws1, ws2])
	),
	cleanup((
		nb_delete(workspaces),
		retractall(workspaces(_))
	))
]) :-
	assertion(set(workspaces, [ws0, ws1, ws2, ws3, ws4])),
	assertion(setting:set_workspaces)
.

test("init_config (dry run on empty config)") :-
	assertion(setting:init_config(true))
.

test("init_config (dry run on default config)") :-
	assertion(forall(setting:default_set(Setting, DefValue), setting:store_setting(Setting, DefValue))),
	assertion(setting:init_config(true))
.

test("init_config (normal run)") :-
	assertion(setting:init_config(false)),

	assertion(forall(setting:setting(Setting), (
		setting:default_set(Setting, DefValue),
		findall(Value, call(setting:Setting, Value), [DefValue])
	)))
.

