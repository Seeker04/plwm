% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

% helpers
dump_to_file(Selection) :- open("/tmp/test-output", write, S), writeln(S, Selection), close(S).
dump_to_file(_, Selection) :- open("/tmp/test-output", write, S), writeln(S, Selection), close(S).

:- begin_tests(menu_tests).

:- use_module("../../src/menu").

test("spawn_menu", [
	setup(
		assertz(menu:menucmd(["tail"])) % select last line(s)
	),
	cleanup((
		retract(menu:menucmd(_)),
		delete_file("/tmp/test-output")
	))
]) :-
	% pass -1 to `tail` to select the last line (menucmd/1 is mocked)
	assertion(menu:spawn_menu("-1", ["a", "b", "c", "d", "e"], dump_to_file)),

	% check if the callback (dump_to_file) was executed with the proper selection
	open("/tmp/test-output", read, S),
	assertion(read_string(S, _, "[e]\n")),
	close(S)
.

test("read_from_prompt", [
	setup(
		assertz(menu:menucmd(["echo", "test_input"]))
	),
	cleanup((
		retract(menu:menucmd(_))
	))
]) :-
	% prompt is appended as last argument to `echo test_input` (menucmd/1 is mocked)
	assertion(menu:read_from_prompt("prompt", "test_input prompt"))
.

test("mon_ws_format + (1 mon, 1 ws)", [
	setup((
		assertz(menu:monitors(["eDP-1"])),
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_format("eDP-1", foo, "")) % nowhere to switch to
.

test("mon_ws_format + (1 mon, 1< ws)", [
	setup((
		assertz(menu:monitors(["eDP-1"])),
		compat_nb_setval(workspaces, [foo, bar, baz])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_format("eDP-1", foo, "foo")),
	assertion(menu:mon_ws_format("eDP-1", bar, "bar")),
	assertion(menu:mon_ws_format("eDP-1", baz, "baz"))
.

test("mon_ws_format + (1< mon, 1 ws)", [
	setup((
		assertz(menu:monitors(["eDP-1", "HDMI-1", "HDMI-2"])),
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_format("eDP-1", foo, "eDP-1")),
	assertion(menu:mon_ws_format("HDMI-1", foo, "HDMI-1")),
	assertion(menu:mon_ws_format("HDMI-2", foo, "HDMI-2"))
.

test("mon_ws_format + (1< mon, 1< ws)", [
	setup((
		assertz(menu:monitors(["eDP-1", "HDMI-1", "HDMI-2"])),
		compat_nb_setval(workspaces, [foo, bar, baz])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_format("eDP-1", foo, "eDP-1 / foo")),
	assertion(menu:mon_ws_format("eDP-1", bar, "eDP-1 / bar")),
	assertion(menu:mon_ws_format("eDP-1", baz, "eDP-1 / baz")),
	assertion(menu:mon_ws_format("HDMI-1", foo, "HDMI-1 / foo")),
	assertion(menu:mon_ws_format("HDMI-1", bar, "HDMI-1 / bar")),
	assertion(menu:mon_ws_format("HDMI-1", baz, "HDMI-1 / baz")),
	assertion(menu:mon_ws_format("HDMI-2", foo, "HDMI-2 / foo")),
	assertion(menu:mon_ws_format("HDMI-2", bar, "HDMI-2 / bar")),
	assertion(menu:mon_ws_format("HDMI-2", baz, "HDMI-2 / baz"))
.

test("mon_ws_format - (empty monitors)", [
	setup((
		assertz(menu:monitors([])),
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_format(_, _, _))
.

test("mon_ws_format - (empty workspaces)", [
	setup((
		assertz(menu:monitors(["eDP-1"])),
		compat_nb_setval(workspaces, [])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_format(_, _, _))
.

test("mon_ws_format - (empty monitors & empty workspaces)", [
	setup((
		assertz(menu:monitors([])),
		compat_nb_setval(workspaces, [])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_format(_, _, _))
.

test("mon_ws_format - (monitors undefined)", [
	setup((
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_format(_, _, _))
.

test("mon_ws_format - (workspaces undefined)", [
	setup((
		assertz(menu:monitors(["eDP-1"]))
	)),
	cleanup((
		retract(menu:monitors(_))
	)),
	throws(_)
]) :-
	menu:mon_ws_format(_, _, _)
.

test("mon_ws_format - (monitors & workspaces undefined)") :-
	assertion(\+ menu:mon_ws_format(_, _, _))
.

test("mon_ws_wint_format + (1 mon, 1 ws)", [
	setup((
		assertz(menu:monitors(["eDP-1"])),
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_wint_format("eDP-1", foo, "win", "win")) % nowhere to switch to
.

test("mon_ws_wint_format + (1 mon, 1< ws)", [
	setup((
		assertz(menu:monitors(["eDP-1"])),
		compat_nb_setval(workspaces, [foo, bar, baz])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_wint_format("eDP-1", foo, "win", "foo    win")),
	assertion(menu:mon_ws_wint_format("eDP-1", bar, "win", "bar    win")),
	assertion(menu:mon_ws_wint_format("eDP-1", baz, "win", "baz    win"))
.

test("mon_ws_wint_format + (1< mon, 1 ws)", [
	setup((
		assertz(menu:monitors(["eDP-1", "HDMI-1", "HDMI-2"])),
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_wint_format("eDP-1",  foo, "win", "eDP-1    win")),
	assertion(menu:mon_ws_wint_format("HDMI-1", foo, "win", "HDMI-1    win")),
	assertion(menu:mon_ws_wint_format("HDMI-2", foo, "win", "HDMI-2    win"))
.

test("mon_ws_wint_format + (1< mon, 1< ws)", [
	setup((
		assertz(menu:monitors(["eDP-1", "HDMI-1", "HDMI-2"])),
		compat_nb_setval(workspaces, [foo, bar, baz])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(menu:mon_ws_wint_format("eDP-1",  foo, "win", "eDP-1 / foo    win")),
	assertion(menu:mon_ws_wint_format("eDP-1",  bar, "win", "eDP-1 / bar    win")),
	assertion(menu:mon_ws_wint_format("eDP-1",  baz, "win", "eDP-1 / baz    win")),
	assertion(menu:mon_ws_wint_format("HDMI-1", foo, "win", "HDMI-1 / foo    win")),
	assertion(menu:mon_ws_wint_format("HDMI-1", bar, "win", "HDMI-1 / bar    win")),
	assertion(menu:mon_ws_wint_format("HDMI-1", baz, "win", "HDMI-1 / baz    win")),
	assertion(menu:mon_ws_wint_format("HDMI-2", foo, "win", "HDMI-2 / foo    win")),
	assertion(menu:mon_ws_wint_format("HDMI-2", bar, "win", "HDMI-2 / bar    win")),
	assertion(menu:mon_ws_wint_format("HDMI-2", baz, "win", "HDMI-2 / baz    win"))
.

test("mon_ws_wint_format - (empty monitors)", [
	setup((
		assertz(menu:monitors([])),
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_wint_format(_, _, _, _))
.

test("mon_ws_wint_format - (empty workspaces)", [
	setup((
		assertz(menu:monitors(["eDP-1"])),
		compat_nb_setval(workspaces, [])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_wint_format(_, _, _, _))
.

test("mon_ws_wint_format - (empty monitors & empty workspaces)", [
	setup((
		assertz(menu:monitors([])),
		compat_nb_setval(workspaces, [])
	)),
	cleanup((
		retract(menu:monitors(_)),
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_wint_format(_, _, _, _))
.

test("mon_ws_wint_format - (monitors undefined)", [
	setup((
		compat_nb_setval(workspaces, [foo])
	)),
	cleanup((
		nb_delete(workspaces)
	))
]) :-
	assertion(\+ menu:mon_ws_wint_format(_, _, _, _))
.

test("mon_ws_wint_format - (workspaces undefined)", [
	setup((
		assertz(menu:monitors(["eDP-1"]))
	)),
	cleanup((
		retract(menu:monitors(_))
	)),
	throws(_)
]) :-
	menu:mon_ws_wint_format(_, _, _, _)
.

test("mon_ws_wint_format - (monitors & workspaces undefined)") :-
	assertion(\+ menu:mon_ws_wint_format(_, _, _, _))
.

test("spawn_winlist_menu", [
	setup((
		XA_WM_NAME is 39,
		assertz(menu:menucmd(["tail"])), % select last line(s)
		assertz(menu:monitors(["eDP-1", "HDMI-1", "HDMI-2"])),
		assertz(menu:monws_keys([
			 "eDP-1"-foo,  "eDP-1"-bar,  "eDP-1"-baz,
			"HDMI-1"-foo, "HDMI-1"-bar, "HDMI-1"-baz,
			"HDMI-2"-foo, "HDMI-2"-bar, "HDMI-2"-baz
		])),
		assertz(plx:x_get_text_property(0x7fffffffe2ec, 10, "win1", XA_WM_NAME, 1)),
		assertz(plx:x_get_text_property(0x7fffffffe2ec, 11, "win2", XA_WM_NAME, 1)),
		assertz(plx:x_get_text_property(0x7fffffffe2ec, 12, "win3", XA_WM_NAME, 1)),
		list_to_assoc(["eDP-1"-foo-[10, 11, 12]], Assoc),
		compat_nb_setval(workspaces, [foo, bar, baz]),
		compat_nb_setval(windows, Assoc)
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retract(menu:monitors(_)),
		retract(menu:monws_keys(_)),
		retractall(plx:x_get_text_property(_, _, _, _, _)),
		nb_delete(workspaces),
		nb_delete(windows),
		delete_file("/tmp/test-output")
	))
]) :-
	% pass -1 to `tail` to select the last line (menucmd/1 is mocked)
	assertion(menu:spawn_winlist_menu("-1", dump_to_file)),

	% check if the callback (dump_to_file) was executed with the proper selection
	open("/tmp/test-output", read, S),
	assertion(read_string(S, _, "[eDP-1 / foo    win3]\n")),
	close(S)
.

test("goto_workspace", [
	setup((
		compat_nb_setval(workspaces, [foo, bar]),
		assertz(menu:monitors(["eDP-1", "HDMI-1"])),
		assertz(menu:monws_keys(["eDP-1"-foo, "eDP-1"-bar, "HDMI-1"-foo, "HDMI-1"-bar])),
		assertz(menu:active_mon_ws("eDP-1", foo)),
		assertz(menu:menucmd(["sh", "-c", "sed -n 1p"])),
		assertz(menu:switch_monitor(Mon) :- (
			phrase(format_("echo 'switch_monitor(~s)' >> /tmp/test-output", [Mon]), Cmd),
			shell(Cmd)
		)),
		assertz(menu:switch_workspace(Ws) :- (
			phrase(format_("echo 'switch_workspace(~a)' >> /tmp/test-output", [Ws]), Cmd),
			shell(Cmd)
		))
	)),
	cleanup((
		nb_delete(workspaces),
		retract(menu:monitors(_)),
		retractall(menu:monws_keys(_)),
		retractall(menu:active_mon_ws(_)),
		retract(menu:menucmd(_)),
		retractall(menu:switch_monitor(_)),
		retractall(menu:switch_workspace(_)),
		delete_file("/tmp/test-output")
	))
]) :-
	% Active mon-ws is mocked to be 1-foo, so its absent from the list, so
	% 1-bar must become the first which we select (see `sed -n 1p` above)
	assertion(menu:goto_workspace),
	sleep(0.1), % wait for file creation to complete (shellcmd runs in bg)
	open("/tmp/test-output", read, S),
	assertion(read_string(S, _, "switch_monitor(eDP-1)\nswitch_workspace(bar)\n"))
.

% TODO: goto_window

% TODO: pull_from

% TODO: push_to

% TODO: close_windows

% TODO: keep_windows

% TODO: create_workspace

% TODO: rename_workspace

% TODO: reindex_workspace

% TODO: delete_workspaces

test("cmd_desc +") :- % no point in testing this, simply check that it's defined (with a simple case)
	assertion(menu:cmd_desc(quit, "Quit plwm"))
.

test("cmd_desc -") :-
	assertion(\+ menu:cmd_desc(i_dont_exist, _)) % nonexistent cmd name
.

test("keybind_padded +") :-
	% realistic kebinds
	assertion(menu:keybind_padded([a], [a])),
	assertion(menu:keybind_padded([ctrl, '+', a], [ctrl, ' ', '+', ' ', a])),
	assertion(menu:keybind_padded([ctrl, '+', shift, '+', 'c'], [ctrl, ' ', '+', ' ', shift, ' ', '+', ' ', c])),

	% unrealistic kebinds
	assertion(menu:keybind_padded([], [])),
	assertion(menu:keybind_padded([a, b], [a, b])),
	assertion(menu:keybind_padded([a, b, c], [a, b, c])),
	assertion(menu:keybind_padded(['+'], [' ', '+', ' '])),
	assertion(menu:keybind_padded([a, '+'], [a, ' ', '+', ' '])),
	assertion(menu:keybind_padded(['+', a], [' ', '+', ' ', 'a'])),
	assertion(menu:keybind_padded([a, '+', '+'], [a, ' ', '+', ' ', ' ', '+', ' '])),
	assertion(menu:keybind_padded(['+', '+', a], [' ', '+', ' ', ' ', '+', ' ', a])),
	assertion(menu:keybind_padded([a, '+', '+', b], [a, ' ', '+', ' ', ' ', '+', ' ', b]))
.

test("keybind_padded -") :-
	assertion(\+ menu:keybind_padded([], [_|_])),
	assertion(\+ (menu:keybind_padded([A], [B]), A \= B)),
	assertion(\+ (menu:keybind_padded([A, '+', B], [C, ' ', '+', ' ', D]), A \= C, B \= D))
.

test("change_nmaster_prompt + (set)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "5"])),
		% change_nmaster_prompt ignores result of change_nmaster, so simulate issue with throw
		assertz(menu:change_nmaster(N) :- \+ (integer(N) ; (N = +M, integer(M))) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_nmaster(_))
	))
]) :-
	assertion(menu:change_nmaster_prompt)
.

test("change_nmaster_prompt + (increment)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "+5"])),
		% change_nmaster_prompt ignores result of change_nmaster, so simulate issue with throw
		assertz(menu:change_nmaster(N) :- \+ (integer(N) ; (N = +M, integer(M))) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_nmaster(_))
	))
]) :-
	assertion(menu:change_nmaster_prompt)
.

test("change_nmaster_prompt + (decrement)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "-5"])),
		% change_nmaster_prompt ignores result of change_nmaster, so simulate issue with throw
		assertz(menu:change_nmaster(N) :- \+ (integer(N) ; (N = +M, integer(M))) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_nmaster(_))
	))
]) :-
	assertion(menu:change_nmaster_prompt)
.

test("change_nmaster_prompt -", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "foo"])),
		% change_nmaster_prompt ignores result of change_nmaster, so simulate issue with throw
		assertz(menu:change_nmaster(N) :- \+ (integer(N) ; (N = +M, integer(M))) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_nmaster(_))
	)),
	throws(err)
]) :-
	menu:change_nmaster_prompt
.

test("change_mfact_prompt + (set - decimal form)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "0.5"])),
		% change_mfact_prompt ignores result of change_mfact, so simulate issue with throw
		assertz(menu:change_mfact(F) :-
			\+ (float(F) ; (F = +G, float(G)) ; (F = N/D, integer(N), integer(D))) -> throw(err)
		)
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_mfact(_))
	))
]) :-
	assertion(menu:change_mfact_prompt)
.

test("change_mfact_prompt + (set - fraction form)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "1/2"])),
		% change_mfact_prompt ignores result of change_mfact, so simulate issue with throw
		assertz(menu:change_mfact(F) :-
			\+ (float(F) ; (F = +G, float(G)) ; (F = N/D, integer(N), integer(D))) -> throw(err)
		)
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_mfact(_))
	))
]) :-
	assertion(menu:change_mfact_prompt)
.

test("change_mfact_prompt + (increment)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "+0.5"])),
		% change_mfact_prompt ignores result of change_mfact, so simulate issue with throw
		assertz(menu:change_mfact(F) :-
			\+ (float(F) ; (F = +G, float(G)) ; (F = N/D, integer(N), integer(D))) -> throw(err)
		)
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_mfact(_))
	))
]) :-
	assertion(menu:change_mfact_prompt)
.

test("change_mfact_prompt + (decrement)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "-0.5"])),
		% change_mfact_prompt ignores result of change_mfact, so simulate issue with throw
		assertz(menu:change_mfact(F) :-
			\+ (float(F) ; (F = +G, float(G)) ; (F = N/D, integer(N), integer(D))) -> throw(err)
		)
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_mfact(_))
	))
]) :-
	assertion(menu:change_mfact_prompt)
.

test("change_mfact_prompt -", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "foo"])),
		% change_mfact_prompt ignores result of change_mfact, so simulate issue with throw
		assertz(menu:change_mfact(F) :-
			\+ (float(F) ; (F = +G, float(G)) ; (F = N/D, integer(N), integer(D))) -> throw(err)
		)
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retractall(menu:change_mfact(_))
	)),
	throws(err)
]) :-
	menu:change_mfact_prompt
.

test("shellcmd_prompt +", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "echo foo > /tmp/test-output"]))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		delete_file("/tmp/test-output")
	))
]) :-
	assertion(menu:shellcmd_prompt),
	sleep(0.1), % wait for file creation to complete (shellcmd runs in bg)
	open("/tmp/test-output", read, S),
	assertion(read_string(S, _, "foo\n"))
.

% Note: shellcmd should *never* fail or throw an exception, so expect neither
test("shellcmd_prompt - (invalid shell command)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "echooo 2>/dev/null"]))
	)),
	cleanup((
		retract(menu:menucmd(_))
	))
]) :-
	assertion(menu:shellcmd_prompt)
.

test("change_setting_prompt + (set)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "3"])),
		% change_setting_prompt ignores result of set, so simulate issue with throw
		assertz(menu:set(border_width, N) :- \+ integer(N) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retract(menu:set(_, _))
	))
]) :-
	assertion(menu:change_setting_prompt(border_width, false))
.

test("change_setting_prompt - (set)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "foo"])),
		% change_setting_prompt ignores result of set, so simulate issue with throw
		assertz(menu:set(border_width, N) :- \+ integer(N) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retract(menu:set(_, _))
	)),
	throws(err)
]) :-
	menu:change_setting_prompt(border_width, false)
.

test("change_setting_prompt + (add)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "start -> writeln(foo)"])),
		% change_setting_prompt ignores result of add, so simulate issue with throw
		assertz(menu:add(hooks, H) :- \+ (H = (Event -> _), atom(Event)) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retract(menu:add(_, _))
	))
]) :-
	assertion(menu:change_setting_prompt(hooks, true))
.

test("change_setting_prompt - (add)", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "foo"])),
		% change_setting_prompt ignores result of add, so simulate issue with throw
		assertz(menu:add(hooks, H) :- \+ (H = (Event -> _), atom(Event)) -> throw(err))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retract(menu:add(_, _))
	)),
	throws(err)
]) :-
	menu:change_setting_prompt(hooks, true)
.

test("dump_settings_prompt", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "somefile"])),
		assertz(menu:dump_settings(FilePath, false) :- string(FilePath))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retract(menu:dump_settings(_, _))
	))
]) :-
	assertion(menu:dump_settings_prompt)
.

test("dump_changed_settings_prompt", [
	setup((
		% we omit from the output the prompt read_from_prompt/0 appends
		assertz(menu:menucmd(["sh", "-c", "echo $0", "somefile"])),
		assertz(menu:dump_settings(FilePath, true) :- string(FilePath))
	)),
	cleanup((
		retract(menu:menucmd(_)),
		retract(menu:dump_settings(_, _))
	))
]) :-
	assertion(menu:dump_changed_settings_prompt)
.

test("run_cmd +") :-
	assertion(menu:run_cmd([false-"foo", true-"selected", false-"bar"], ["selected"])),
	assertion(menu:run_cmd([false-"foo", false-"bar"], ["selected"]))
.

test("run_cmd -") :-
	assertion(\+ menu:run_cmd([true-"foo", false-"selected", true-"bar"], ["selected"])),
	assertion(\+ menu:run_cmd(_, [])) % no selection
.

test("list_keymaps", [
	setup((
		assertz(menu:keymaps([
			super +         j -> shellcmd("echo super + j         > /tmp/test-output-1"),
			super + shift + j -> shellcmd("echo super + shift + j > /tmp/test-output-2")
		]))
	)),
	cleanup((
		retractall(menu:keymaps(_)),
		delete_file("/tmp/test-output-1"),
		delete_file("/tmp/test-output-2")
	))
]) :-
	assertz(menu:menucmd(["sh", "-c", "sed -n 1p"])),
	assertion(menu:list_keymaps),
	sleep(0.1), % wait for file creation to complete (shellcmd runs in bg)
	open("/tmp/test-output-1", read, S),
	assertion(read_string(S, _, "super + j\n")),
	retract(menu:menucmd(_)),

	assertz(menu:menucmd(["sh", "-c", "sed -n 2p"])),
	assertion(menu:list_keymaps),
	sleep(0.1),
	open("/tmp/test-output-2", read, S2),
	assertion(read_string(S2, _, "super + shift + j\n")),
	retract(menu:menucmd(_))
.

test("list_cmds", [
	setup((
		assertz(menu:shift_focus(down) :- dump_to_file("focus down called")),
		assertz(menu:shift_focus(up) :- dump_to_file("focus up called")),
		assertz(menu:monitors([])),
		compat_nb_setval(workspaces, [])
	)),
	cleanup((
		retractall(menu:shift_focus(_)),
		retract(menu:monitors(_)),
		nb_delete(workspaces),
		delete_file("/tmp/test-output")
	))
]) :-
	assertz(menu:menucmd(["sh", "-c", "sed -n 1p"])),
	assertion(menu:list_cmds),
	sleep(0.1), % wait for file creation to complete (shellcmd runs in bg)
	open("/tmp/test-output", read, S),
	assertion(read_string(S, _, "focus down called\n")),
	retract(menu:menucmd(_)),

	assertz(menu:menucmd(["sh", "-c", "sed -n 2p"])),
	assertion(menu:list_cmds),
	sleep(0.1),
	open("/tmp/test-output", read, S2),
	assertion(read_string(S2, _, "focus up called\n")),
	retract(menu:menucmd(_))
.

:- end_tests(menu_tests).
