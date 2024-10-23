% MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

:- module(menu, []).

spawn_menu(_, [], _) :- !.
spawn_menu(Prompt, Entries, Callback) :-
	optcnf_then(menucmd([MenuCmd|MenuArgs]), (
		append(MenuArgs, [Prompt], MenuArgsWithPrompt),

		process:process_create(path(MenuCmd), MenuArgsWithPrompt, [stdin(pipe(MenuIn)), stdout(pipe(MenuOut))]),

		forall(member(Entry, Entries), writeln(MenuIn, Entry)), close(MenuIn),
		read_string(MenuOut, Len, MenuOutStr),
		(1 < Len ->
			split_string(MenuOutStr, "\n", "\n", SelectedLines),
			(forall(member(Line, SelectedLines), member(Line, Entries)) ->
				ignore(call(Callback, SelectedLines))
			; true)
			% don't accept arbitrary input from menu prompt, only proper selection
		; true)
	))
.

read_from_prompt(Prompt, Input) :-  % use menucmd as a simple input prompt
	optcnf_then_else(menucmd([MenuCmd|MenuArgs]),
		(append(MenuArgs, [Prompt], MenuArgsWithPrompt),

		process:process_create(path(MenuCmd), MenuArgsWithPrompt, [stdin(pipe(MenuIn)), stdout(pipe(MenuOut))]),
		close(MenuIn), % no input for menu
		read_string(MenuOut, _, InputLine), InputLine \= "", InputLine \= "\n",
		utils:str_withoutlastch(InputLine, Input))
	,
		fail
	)
.

mon_ws_format(Mon, Ws, Str) :-
	monitors(Mons), nb_getval(workspaces, Wss), length(Mons, MonCnt), length(Wss, WsCnt),
	(1 == MonCnt, 1 == WsCnt -> Str = ""
	;1 == MonCnt, 1 <  WsCnt -> format(string(Str),      "~a", [     Ws])
	;1 <  MonCnt, 1 == WsCnt -> format(string(Str), "~d"     , [Mon    ])
	;1 <  MonCnt, 1 <  WsCnt -> format(string(Str), "~d / ~a", [Mon, Ws]))
.

mon_ws_wint_format(Mon, Ws, WinT, Str) :-
	monitors(Mons), nb_getval(workspaces, Wss), length(Mons, MonCnt), length(Wss, WsCnt),
	maplist(atom_length, Wss, WsWidths),
	max_list(WsWidths, WsMaxWidth),
	(1 == MonCnt, 1 == WsCnt -> format(string(Str),                    "~s", [WinT])
	;1 <  MonCnt, 1 == WsCnt -> format(string(Str),              "~d    ~s", [Mon, WinT])
	;1 == MonCnt, 1 <  WsCnt -> format(string(Fmt),       "~~a~~~d|    ~~s", [WsMaxWidth]),
	                            format(string(Str), Fmt, [Ws, WinT])
	;1 <  MonCnt, 1 <  WsCnt -> format(string(Fmt), "~~d / ~~a~~~d|    ~~s", [WsMaxWidth+4]),
		                    format(string(Str), Fmt, [Mon, Ws, WinT]))
.


%*********************    Navigation/window placement    **********************

goto_workspace() :-
	monws_keys(Keys), active_mon_ws(ActMon, ActWs),
	findall(Mon-Ws-MenuEntry, (   % map key (Mon-Ws) to lines for later lookup
		member(Mon-Ws, Keys),
		Mon-Ws \= ActMon-ActWs,
		mon_ws_format(Mon, Ws, MenuEntry)),
		MenuEntries),
	findall(Line, member(_-_-Line, MenuEntries), Lines),
	spawn_menu("goto workspace", Lines, menu:goto_workspace_(MenuEntries))
.
goto_workspace_(MenuEntries, [Selection]) :-
	(member(Mon-Ws-Selection, MenuEntries) ->
		switch_monitor(Mon),
		switch_workspace(Ws)
	; true)
.

goto_window() :-
	display(Dp), monws_keys(Keys), XA_WM_NAME is 39,
	findall(MenuEntries, (
		member(Mon-Ws, Keys), global_key_value(windows, Mon-Ws, Wins),
		findall(Win-MenuEntry, (   % map XID to lines for later lookup
			member(Win, Wins),
			plx:x_get_text_property(Dp, Win, WinTitle, XA_WM_NAME, Status), Status =\= 0,
			mon_ws_wint_format(Mon, Ws, WinTitle, MenuEntry)),
			MenuEntries)),
		MenuEntriesAll
	),
	flatten(MenuEntriesAll, MenuInput),
	findall(Line, member(Win-Line, MenuInput), Lines),
	spawn_menu("goto window", Lines, menu:goto_window_(MenuInput))
.
goto_window_(MenuInput, [Selection]) :-
	(member(Win-Selection, MenuInput) ->
		win_mon_ws(Win, TargetMon, TargetWs),
		switch_monitor(TargetMon),
		switch_workspace(TargetWs),
		unfocus_onlyvisual(false), focus(Win), raise(Win)
	; true)
.

pull_from() :-
	display(Dp), active_mon_ws(ActMon, ActWs), monws_keys(Keys),
	findall(MenuEntries, (
		member(Mon-Ws, Keys), Mon-Ws \= ActMon-ActWs, global_key_value(windows, Mon-Ws, Wins),
		findall(Win-MenuEntry, (   % map XID to lines for later lookup
			member(Win, Wins),
			XA_WM_NAME is 39,
			plx:x_get_text_property(Dp, Win, WinTitle, XA_WM_NAME, Status), Status =\= 0,
			mon_ws_wint_format(Mon, Ws, WinTitle, MenuEntry)),
			MenuEntries)),
		MenuEntriesAll
	),
	flatten(MenuEntriesAll, MenuInput),
	findall(Line, member(Win-Line, MenuInput), Lines),
	spawn_menu("pull from", Lines, menu:pull_from_(MenuInput))
.
pull_from_(MenuInput, Selections) :-
	forall(member(Sel, Selections), (
		(member(Win-Sel, MenuInput) ->
			active_mon_ws(ActMon, ActWs),
			win_tomon_toworkspace_top(Win, ActMon, ActWs, true),
			focus(Win), raise(Win)
		; true)
	))
.

push_to() :-
	global_value(focused, FocusedWin),
	(FocusedWin =\= 0 ->
		monws_keys(Keys), active_mon_ws(ActMon, ActWs),
		findall(Mon-Ws-MenuEntry, (   % map key (Mon-Ws) to lines for later lookup
			member(Mon-Ws, Keys),
			Mon-Ws \= ActMon-ActWs,
			mon_ws_format(Mon, Ws, MenuEntry)),
			MenuEntries),
		findall(Line, member(_-_-Line, MenuEntries), Lines),
		spawn_menu("push to", Lines, menu:push_to_(MenuEntries))
	; true)
.
push_to_(MenuEntries, [Selection]) :-
	(member(Mon-Ws-Selection, MenuEntries) ->
		global_value(focused, FocusedWin),
		(FocusedWin =\= 0 ->
			win_tomon_toworkspace_top(FocusedWin, Mon, Ws, true)
		; true)
	; true)
.


%*********************  "Dynamic workspaces" operations  **********************

create_workspace() :-
	(read_from_prompt("new workspace", Input) ->
		atom_string(Ws, Input),
		create_workspace(Ws)
	; true)
.

rename_workspace() :- % will rename the active one
	active_mon_ws(_, ActWs),
	(read_from_prompt("rename workspace to", Input) ->
		atom_string(Ws, Input),
		rename_workspace(ActWs, Ws)
	; true)
.

reindex_workspace() :- % will reindex the active one
	active_mon_ws(_, ActWs), nb_getval(workspaces, Wss), nth1(ActIdx, Wss, ActWs),
	length(Wss, WsCnt),
	(1 < WsCnt ->
		findall(IStr, (between(1, WsCnt, I), I =\= ActIdx, number_string(I, IStr)), Lines),
		spawn_menu("reindex workspace to", Lines, menu:reindex_workspace_(ActWs))
	; true)
.
reindex_workspace_(Ws, [Selection]) :- number_string(Idx, Selection), reindex_workspace(Ws, Idx).

delete_workspace() :-
	nb_getval(workspaces, Wss),
	(Wss \= [_] ->  % don't even spawn the list if there is only one ws left
		findall(WsStr, (member(Ws, Wss), atom_string(Ws, WsStr)), Lines),
		spawn_menu("delete workspace", Lines, menu:delete_workspace_)
	; true)
.
delete_workspace_(Selections) :-
	forall(member(Sel, Selections), (
		atom_string(Ws, Sel), delete_workspace(Ws)
	))
.


%*********************************  Extras  ***********************************

cmd_desc(shift_focus(down) , "Focus next window in stack").
cmd_desc(shift_focus(up)   , "Focus previous window in stack").
cmd_desc(move_focused(down), "Swap focused window with the next").
cmd_desc(move_focused(up)  , "Swap focused window with the preceding").
cmd_desc(focused_to_top    , "Move focused window to top of the stack").
cmd_desc(close_focused     , "Close focused window").
cmd_desc(toggle_floating   , "Manage/unmanage focused window").
cmd_desc(toggle_fullscreen , "Toggle fullscreen of focused window").
cmd_desc(quit              , "Quit plwm").
cmd_desc(change_nmaster(N) , D) :-
	(N = +Delta, integer(Delta) -> format(string(D), "Increase number of master windows by ~d", [Delta])
	;integer(N), N < 0          -> format(string(D), "Decrease number of master windows by ~d", [abs(N)])
	;integer(N)                 -> format(string(D), "Set number of master windows to ~d", [N])
	;                              D = "Error: invalid argument").
cmd_desc(change_nmaster, "Set number of master windows").
cmd_desc(change_mfact(F) , D) :-
	(F = +Delta, float(Delta) -> format(string(D), "Add ~0f% to the space of master area", [Delta*100])
	;float(F), F < 0          -> format(string(D), "Remove ~0f% from the space of master area", [abs(F)*100])
	;utils:is_float(F), 0.05 =< F, F =< 0.95 -> format(string(D), "Set master area to ~0f%", [F*100])
	;                            D = "Error: invalid argument").
cmd_desc(change_mfact, "Set master area").
cmd_desc(layout:set_layout(L), D) :- format(string(D), "Switch to ~p layout", [L]).
cmd_desc(toggle_workspace    , "Switch between last two workspaces").
cmd_desc(toggle_hide_empty_workspaces, "Toggle the hide_empty_workspaces setting").
cmd_desc(switch_workspace(prev), "Go to previous workspace")                    :- !.
cmd_desc(switch_workspace(next), "Go to next workspace")                        :- !.
cmd_desc(switch_workspace(prev_nonempty), "Go to previous non-empty workspace") :- !.
cmd_desc(switch_workspace(next_nonempty), "Go to next non-empty workspace")     :- !.
cmd_desc(switch_workspace(N) , D) :- integer(N), format(string(D), "Go to workspace #~d", [N]), !.
cmd_desc(switch_workspace(W) , D) :- format(string(D), "Go to workspace ~p", [W]).
cmd_desc(move_focused_to_workspace(prev), "Move focused window to previous workspace")          :- !.
cmd_desc(move_focused_to_workspace(next), "Move focused window to next workspace")              :- !.
cmd_desc(move_focused_to_workspace(prev_nonempty), "Move focused window to previous workspace") :- !.
cmd_desc(move_focused_to_workspace(next_nonempty), "Move focused window to next workspace")     :- !.
cmd_desc(move_focused_to_workspace(N) , D) :- integer(N), format(string(D), "Move focused window to workspace #~d", [N]), !.
cmd_desc(move_focused_to_workspace(W) , D) :- format(string(D), "Move focused window to workspace ~p", [W]).
cmd_desc(switch_monitor(prev) , "Switch to previous monitor").
cmd_desc(switch_monitor(next) , "Switch to next monitor").
cmd_desc(switch_monitor(prev_nonempty), "Switch to previous non-empty monitor").
cmd_desc(switch_monitor(next_nonempty), "Switch to next non-empty monitor").
cmd_desc(switch_monitor(left) , "Switch monitor in left direction").
cmd_desc(switch_monitor(right), "Switch monitor in right direction").
cmd_desc(switch_monitor(up)   , "Switch monitor in up direction").
cmd_desc(switch_monitor(down) , "Switch monitor in down direction").
cmd_desc(switch_monitor(N) , D) :- integer(N), format(string(D), "Switch to monitor #~d", [N]).
cmd_desc(move_focused_to_monitor(prev) , "Move focused window to previous monitor").
cmd_desc(move_focused_to_monitor(next) , "Move focused window to next monitor").
cmd_desc(move_focused_to_monitor(prev_nonempty), "Move focused window to previous non-empty monitor").
cmd_desc(move_focused_to_monitor(next_nonempty), "Move focused window to next non-empty monitor").
cmd_desc(move_focused_to_monitor(left) , "Move focused window to monitor in left direction").
cmd_desc(move_focused_to_monitor(right), "Move focused window to monitor in right direction").
cmd_desc(move_focused_to_monitor(up)   , "Move focused window to monitor in up direction").
cmd_desc(move_focused_to_monitor(down) , "Move focused window to monitor in down direction").
cmd_desc(move_focused_to_monitor(N) , D) :- integer(N), format(string(D), "Moved focused window to monitor #~d", [N]).
cmd_desc(menu:goto_window      , "Go to selected window, raise and focus it").
cmd_desc(menu:goto_workspace   , "Go to selected workspace").
cmd_desc(menu:pull_from        , "Pull selected window to active workspace").
cmd_desc(menu:push_to          , "Push focused window to the selected workspace").
cmd_desc(menu:create_workspace , "Create new workspaces").
cmd_desc(menu:rename_workspace , "Rename selected workspace").
cmd_desc(menu:reindex_workspace, "Reindex selected workspace").
cmd_desc(menu:delete_workspace , "Delete selected workspace").
cmd_desc(menu:list_keymaps     , "List all defined keymaps").
cmd_desc(shellcmd(Cmd), D) :- format(string(D), "Run `~s`", [Cmd]).
cmd_desc(shellcmd, "Run a shell command").

keybind_padded([], []).
keybind_padded(['+'|Cs], [' ', '+', ' '|Rest]) :- !, keybind_padded(Cs, Rest).
keybind_padded([C|Cs], [C|Rest]) :- keybind_padded(Cs, Rest).

change_nmaster_prompt() :-
	(read_from_prompt("nmaster (+N, -N or N)", Input) ->
		ignore((catch(term_string(N, Input), Ex, (writeln(Ex), fail)), change_nmaster(N)))
	; true)
.

change_mfact_prompt() :-
	(read_from_prompt("mfact (+F, -F or F)", Input) ->
		ignore((catch(term_string(F, Input), Ex, (writeln(Ex), fail)), change_mfact(F)))
	; true)
.

shellcmd_prompt() :-
	(read_from_prompt("shellcmd", Input) ->
		shellcmd(Input)
	; true)
.

run_cmd(MenuEntries, [Selection]) :-
	(member(Cmd-Selection, MenuEntries) ->
		(Cmd = change_nmaster -> change_nmaster_prompt
		;Cmd = change_mfact   -> change_mfact_prompt
		;Cmd = shellcmd       -> shellcmd_prompt
		;call(Cmd))
	; true)
.

list_keymaps() :-
	config(keymaps(Keymaps)),
	findall(KBStr,
		(member((KB -> _), Keymaps), format(chars(KBChars), "~p", [KB]),
		keybind_padded(KBChars, KBCharsPadded), string_chars(KBStr, KBCharsPadded)),
		KBStrs),
	findall(ActStr,
		(member((_ -> Act), Keymaps), format(chars(ActStr), "~p", [Act])),
		ActStrs),
	maplist(string_length, KBStrs, KBWidths),
	maplist(string_length, ActStrs, ActWidths),
	max_list(KBWidths, KBMaxWidth),
	max_list(ActWidths, ActMaxWidth),
	format(string(Fmt), "~~s~~~d|~~p~~~d|~~s", [KBMaxWidth+3, KBMaxWidth+3+ActMaxWidth+3]),

	findall(Action-Line, (   % map key (Action) to lines for later lookup
		nth1(Idx, Keymaps, (_ -> Action)),
		nth1(Idx, KBStrs, KBStr),
		cmd_desc(Action, Desc),
		format(string(Line), Fmt, [KBStr, Action, Desc])),
		MenuEntries),
	findall(Line, member(_-Line, MenuEntries), Lines),
	spawn_menu("keymaps", Lines, menu:run_cmd(MenuEntries))
.

list_cmds() :-
	findall(layout:set_layout(Layout), layout:is_layout(Layout), SLCmds1),
	delete(SLCmds1, layout:set_layout(nrows(_)), SLCmds2),
	delete(SLCmds2, layout:set_layout(ncols(_)), SLCmds3),
	findall(layout:set_layout(L), member(L, [nrows(2),nrows(3),nrows(4),ncols(2),ncols(3),ncols(4)]), SLCmds4),
	append(SLCmds3, SLCmds4, SetLayoutCmds),

	nb_getval(workspaces, Wss),
	length(Wss, WsCnt),
	findall(switch_workspace(Ws), member(Ws, Wss), SwitchWsByNameCmds),
	findall(switch_workspace(Idx), between(1, WsCnt, Idx), SwitchWsByIdxCmds),
	findall(move_focused_to_workspace(Ws), member(Ws, Wss), MoveToWsByNameCmds),
	findall(move_focused_to_workspace(Idx), between(1, WsCnt, Idx), MoveToWsByIdxCmds),

	monitors(Mons),
	findall(switch_monitor(Mon), member(Mon, Mons), SwitchMonCmds),
	findall(move_focused_to_monitor(Mon), member(Mon, Mons), MoveToMonCmds),

	flatten([
		[
		shift_focus(down),
		shift_focus(up),
		move_focused(down),
		move_focused(up),
		focused_to_top,
		close_focused,
		toggle_floating,
		toggle_fullscreen,
		quit,
		change_nmaster,
		change_mfact
		],
		SetLayoutCmds,
		[
		toggle_workspace,
		toggle_hide_empty_workspaces
		],
		SwitchWsByNameCmds,
		SwitchWsByIdxCmds,
		[
		switch_workspace(prev),
		switch_workspace(next),
		switch_workspace(prev_nonempty),
		switch_workspace(next_nonempty)
		],
		MoveToWsByNameCmds,
		MoveToWsByIdxCmds,
		[
		move_focused_to_workspace(prev),
		move_focused_to_workspace(next),
		move_focused_to_workspace(prev_nonempty),
		move_focused_to_workspace(next_nonempty)
		],
		SwitchMonCmds,
		[
		switch_monitor(prev),
		switch_monitor(next),
		switch_monitor(prev_nonempty),
		switch_monitor(next_nonempty),
		switch_monitor(left),
		switch_monitor(right),
		switch_monitor(up),
		switch_monitor(down)
		],
		MoveToMonCmds,
		[
		move_focused_to_monitor(prev),
		move_focused_to_monitor(next),
		move_focused_to_monitor(prev_nonempty),
		move_focused_to_monitor(next_nonempty),
		move_focused_to_monitor(left),
		move_focused_to_monitor(right),
		move_focused_to_monitor(up),
		move_focused_to_monitor(down),
		menu:goto_window,
		menu:goto_workspace,
		menu:pull_from,
		menu:push_to,
		menu:create_workspace,
		menu:rename_workspace,
		menu:reindex_workspace,
		menu:delete_workspace,
		menu:list_keymaps,
		shellcmd
		]
	], Cmds),

	findall(CmdStr,
		(member(Cmd, Cmds), format(chars(CmdStr), "~p", [Cmd])),
		CmdStrs),
	maplist(string_length, CmdStrs, CmdWidths),
	max_list(CmdWidths, CmdMaxWidth),
	format(string(Fmt), "~~p~~~d|~~s", [CmdMaxWidth+3]),

	findall(Cmd-Line, (   % map key (Cmd) to lines for later lookup
		member(Cmd, Cmds),
		cmd_desc(Cmd, Desc),
		format(string(Line), Fmt, [Cmd, Desc])),
		MenuEntries),
	findall(Line, member(_-Line, MenuEntries), Lines),
	spawn_menu("commands", Lines, menu:run_cmd(MenuEntries))
.

