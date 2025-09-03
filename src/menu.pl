% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

:- module(menu, []).

:- use_module(library(lists)).

:- use_module(layout).
:- use_module(utils).

%! spawn_menu(++Prompt:string, ++Entries:[string], :Callback:callable) is det
%
%  Creates a menu process defined in menucmd/1, writes the list of possible
%  selections to its stdin and if a selection happens (to stdout), passes it to
%  the provided callback function.
%
%  @arg Prompt string appended as final argument to menucmd/1
%  @arg Entries list of single-line strings passed as input to menucmd/1
%  @arg Callback predicate that is called on lines selected from Entries
spawn_menu(_, [], _) :- !.
spawn_menu(Prompt, Entries, Callback) :-
	(menucmd([MenuCmd|MenuArgs]) ->
		append(MenuArgs, [Prompt], MenuArgsWithPrompt),

		process:process_create(path(MenuCmd), MenuArgsWithPrompt, [stdin(pipe(MenuIn)), stdout(pipe(MenuOut))]),

		compat_forall(member(Entry, Entries), writeln(MenuIn, Entry)), close(MenuIn),
		read_string(MenuOut, Len, MenuOutStr),
		(1 < Len ->
			split_string(MenuOutStr, "\n", "\n", SelectedLines),
			(compat_forall(member(Line, SelectedLines), member(Line, Entries)) ->
				ignore(call(Callback, SelectedLines))
			; true)
			% don't accept arbitrary input from menu prompt, only proper selection
		; true)
	; true)
.

%! read_from_prompt(++Prompt:string, --Input:string) is semidet
%
%  Uses menucmd/1 with zero selection to create a graphical prompt and
%  captures the text the user writes into the prompt.
%  Fails if menucmd/1 is not set.
%
%  @arg Prompt string appended as final argument to menucmd/1
%  @arg Input string that the user wrote after the prompt
read_from_prompt(Prompt, Input) :-  % use menucmd as a simple input prompt
	(menucmd([MenuCmd|MenuArgs]) ->
		(append(MenuArgs, [Prompt], MenuArgsWithPrompt),

		process:process_create(path(MenuCmd), MenuArgsWithPrompt, [stdin(pipe(MenuIn)), stdout(pipe(MenuOut))]),
		close(MenuIn), % no input for menu
		read_string(MenuOut, _, InputLine), InputLine \= "", InputLine \= "\n",
		utils:str_withoutlastch(InputLine, Input))
	;
		fail
	)
.

%! mon_ws_format(++Mon:string, ++Ws:atom, -Str:string) is det
%
%  From a monitor and workspace name, formats a selection line for a list.
%  The general format is "<monitor name> / <workspace name>".
%  If there is only one monitor, it is omitted.
%  If there is only one workspace, it is omitted.
%  If there is only one monitor and one workspace, the output is empty.
%
%  @arg Mon monitor name
%  @arg Ws workspace name
%  @arg string output of the formatting
mon_ws_format(Mon, Ws, Str) :-
	monitors(Mons), nb_getval(workspaces, Wss), length(Mons, MonCnt), length(Wss, WsCnt),
	(1 == MonCnt, 1 == WsCnt -> Str = ""
	;1 == MonCnt, 1 <  WsCnt ->compat_format(string(Str),      "~a", [     Ws])
	;1 <  MonCnt, 1 == WsCnt ->compat_format(string(Str), "~s"     , [Mon    ])
	;1 <  MonCnt, 1 <  WsCnt ->compat_format(string(Str), "~s / ~a", [Mon, Ws]))
.

%! mon_ws_wint_format(++Mon:string, ++Ws:atom, ++WinT:string -Str:string) is det
%
%  From a monitor name, workspace name and window title, formats a selection line for a list.
%  The general format is "<monitor name> / <workspace name>    <window title>".
%  If there is only one monitor, it is omitted.
%  If there is only one workspace, it is omitted.
%  If there is only one monitor and one workspace, the output is only the window title.
%
%  @arg Mon monitor name
%  @arg Ws workspace name
%  @arg WinT window title string
%  @arg string output of the formatting
mon_ws_wint_format(Mon, Ws, WinT, Str) :-
	monitors(Mons), nb_getval(workspaces, Wss), length(Mons, MonCnt), length(Wss, WsCnt),
	maplist(atom_length, Wss, WsWidths),
	max_list(WsWidths, WsMaxWidth),
	(1 == MonCnt, 1 == WsCnt ->compat_format(string(Str),                    "~s", [WinT])
	;1 <  MonCnt, 1 == WsCnt ->compat_format(string(Str),              "~s    ~s", [Mon, WinT])
	;1 == MonCnt, 1 <  WsCnt ->compat_format(string(Fmt),       "~~a~~~d|    ~~s", [WsMaxWidth]),
	                           compat_format(string(Str), Fmt, [Ws, WinT])
	;1 <  MonCnt, 1 <  WsCnt ->compat_format(string(Fmt), "~~s / ~~a~~~d|    ~~s", [WsMaxWidth+4]),
		                    compat_format(string(Str), Fmt, [Mon, Ws, WinT]))
.

%! spawn_winlist_menu(++Prompt:string, :Callback:callable) is det
%
%  Lists all windows to the user using spawn_menu/3.
%  If a selection happens, runs the specified callback on it.
%
%  @arg Prompt string appended as final argument to menucmd/1
%  @arg Callback predicate that is called on the list of selected windows
spawn_winlist_menu(Prompt, Callback) :-
	user:display(Dp), monws_keys(Keys), XA_WM_NAME is 39,
	findall(MenuEntries, (
		member(Mon-Ws, Keys), global_key_value(windows, Mon-Ws, Wins),
		findall(Win-MenuEntry, (   % map XID to lines for later lookup
			member(Win, Wins),
			plx:x_get_text_property(Dp, Win, WinTitle, XA_WM_NAME, Status), Status =\= 0,
			mon_ws_wint_format(Mon, Ws, WinTitle, MenuEntry)),
			MenuEntries)),
		MenuEntriesAll
	),
	append(MenuEntriesAll, MenuInput),
	findall(Line, member(Win-Line, MenuInput), Lines),
	spawn_menu(Prompt, Lines, call(Callback, MenuInput))
.

%*********************    Navigation/window placement    **********************

%! goto_workspace() is det
%
%  Lists all monitor-workspace combinations other than the active one using spawn_menu/3.
%  If a selection happens, switches to that monitor-workspace.
goto_workspace :-
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

%! goto_window() is det
%
%  Lists all windows (grouped by monitor-workspace) other than the active one using spawn_menu/3.
%  If a selection happens, switches its monitor-workspace, raises and focuses that window.
goto_window :-
	spawn_winlist_menu("goto window", menu:goto_window_)
.
goto_window_(MenuInput, [Selection]) :-
	(member(Win-Selection, MenuInput) ->
		win_mon_ws(Win, TargetMon, TargetWs),
		switch_monitor(TargetMon),
		switch_workspace(TargetWs),
		unfocus_onlyvisual(false), focus(Win), raise(Win)
	; true)
.

%! pull_from() is det
%
%  Lists all windows other than the ones on the active workspace using spawn_menu/3.
%  If a selection happens, moves the selected window(s) to the active monitor-workspace.
pull_from :-
	user:display(Dp), active_mon_ws(ActMon, ActWs), monws_keys(Keys), XA_WM_NAME is 39,
	findall(MenuEntries, (
		member(Mon-Ws, Keys), Mon-Ws \= ActMon-ActWs, global_key_value(windows, Mon-Ws, Wins),
		findall(Win-MenuEntry, (   % map XID to lines for later lookup
			member(Win, Wins),
			plx:x_get_text_property(Dp, Win, WinTitle, XA_WM_NAME, Status), Status =\= 0,
			mon_ws_wint_format(Mon, Ws, WinTitle, MenuEntry)),
			MenuEntries)),
		MenuEntriesAll
	),
	append(MenuEntriesAll, MenuInput),
	findall(Line, member(Win-Line, MenuInput), Lines),
	spawn_menu("pull from", Lines, menu:pull_from_(MenuInput))
.
pull_from_(MenuInput, Selections) :-
	compat_forall((member(Sel, Selections), member(Win-Sel, MenuInput)), (
		active_mon_ws(ActMon, ActWs),
		win_tomon_toworkspace_top(Win, ActMon, ActWs, true),
		focus(Win), raise(Win)
	))
.

%! push_to() is det
%
%  Lists all monitor-workspace combinations other than the active one using spawn_menu/3.
%  If a selection happens, moves the active window to the selected location.
push_to :-
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

%! close_windows() is det
%
%  Lists all windows using spawn_menu/3.
%  If a selection happens, closes the selected window(s).
close_windows :-
	spawn_winlist_menu("close windows", menu:close_windows_)
.
close_windows_(MenuInput, Selections) :-
	compat_forall((member(Sel, Selections), member(Win-Sel, MenuInput)), close_window(Win))
.

%! keep_windows() is det
%
%  Lists all windows using spawn_menu/3.
%  If a selection happens, closes all windows other than the selected ones.
keep_windows :-
	spawn_winlist_menu("keep windows", menu:keep_windows_)
.
keep_windows_(MenuInput, Selections) :-
	compat_forall((member(Win-Line, MenuInput), \+ member(Line, Selections)), close_window(Win))
.

%*********************  "Dynamic workspaces" operations  **********************

%! create_workspace() is det
%
%  Prompts the user for a new workspace name.
%  If it is non-empty and unique, creates it (i.e. appends to the list of workspaces).
create_workspace :-
	(read_from_prompt("new workspace", Input) ->
		atom_string(Ws, Input),
		create_workspace(Ws)
	; true)
.

%! rename_workspace() is det
%
%  Prompts the user for a new workspace name.
%  If it is non-empty and unique, renames the active workspace to it.
rename_workspace :- % will rename the active one
	active_mon_ws(_, ActWs),
	(read_from_prompt("rename workspace to", Input) ->
		atom_string(Ws, Input),
		rename_workspace(ActWs, Ws)
	; true)
.

%! reindex_workspace() is det
%
%  Lists the possible new workspace indices for the active one.
%  If a selection happens, the active workspace is re-indexed to the new position.
reindex_workspace :- % will reindex the active one
	active_mon_ws(_, ActWs), nb_getval(workspaces, Wss), nth1(ActIdx, Wss, ActWs),
	length(Wss, WsCnt),
	(1 < WsCnt ->
		findall(IStr, (between(1, WsCnt, I), I =\= ActIdx, number_string(I, IStr)), Lines),
		spawn_menu("reindex workspace to", Lines, menu:reindex_workspace_(ActWs))
	; true)
.
reindex_workspace_(Ws, [Selection]) :- number_string(Idx, Selection), reindex_workspace(Ws, Idx).

%! delete_workspaces() is det
%
%  Lists all workspaces.
%  If a selection happens, the selected ones are removed.
%  Note: owned windows of deleted workspaces are moved to the next free workspace in the list.
%  Note: the last workspace is never removed (e.g. if all of them are selected).
delete_workspaces :-
	nb_getval(workspaces, Wss),
	(Wss \= [_] ->  % don't even spawn the list if there is only one ws left
		findall(WsStr, (member(Ws, Wss), atom_string(Ws, WsStr)), Lines),
		spawn_menu("delete workspaces", Lines, menu:delete_workspaces_)
	; true)
.
delete_workspaces_(Selections) :-
	compat_forall(member(Sel, Selections), (atom_string(Ws, Sel), delete_workspace(Ws)))
.


%*********************************  Extras  ***********************************

%! cmd_desc(++Cmd:term, -Desc:string) is det
%
%  Takes a command predicate (i.e. predicate intended to be called by the user)
%  and returns its brief description.
%
%  @arg Cmd command predicate to describe
%  @arg Desc description of the specified command predicate
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
	(N = +Delta, integer(Delta) ->compat_format(string(D), "Increase number of master windows by ~d", [Delta])
	;integer(N), N < 0          ->compat_format(string(D), "Decrease number of master windows by ~d", [abs(N)])
	;integer(N)                 ->compat_format(string(D), "Set number of master windows to ~d", [N])
	;                              D = "Error: invalid argument").
cmd_desc(change_nmaster, "Set number of master windows").
cmd_desc(change_mfact(F) , D) :-
	(F = +Delta, float(Delta) ->compat_format(string(D), "Add ~0f% to the space of master area", [Delta*100])
	;float(F), F < 0          ->compat_format(string(D), "Remove ~0f% from the space of master area", [abs(F)*100])
	;utils:is_float(F), 0.05 =< F, F =< 0.95 ->compat_format(string(D), "Set master area to ~0f%", [F*100])
	;                            D = "Error: invalid argument").
cmd_desc(change_mfact, "Set master area").
cmd_desc(layout:set_layout(L), D) :-compat_format(string(D), "Switch to ~p layout", [L]).
cmd_desc(toggle_workspace    , "Switch between last two workspaces").
cmd_desc(toggle_hide_empty_workspaces, "Toggle the hide_empty_workspaces setting").
cmd_desc(switch_workspace(prev), "Go to previous workspace")                    :- !.
cmd_desc(switch_workspace(next), "Go to next workspace")                        :- !.
cmd_desc(switch_workspace(prev_nonempty), "Go to previous non-empty workspace") :- !.
cmd_desc(switch_workspace(next_nonempty), "Go to next non-empty workspace")     :- !.
cmd_desc(switch_workspace(N) , D) :- integer(N),compat_format(string(D), "Go to workspace #~d", [N]), !.
cmd_desc(switch_workspace(W) , D) :-compat_format(string(D), "Go to workspace ~p", [W]).
cmd_desc(move_focused_to_workspace(prev), "Move focused window to previous workspace")          :- !.
cmd_desc(move_focused_to_workspace(next), "Move focused window to next workspace")              :- !.
cmd_desc(move_focused_to_workspace(prev_nonempty), "Move focused window to previous workspace") :- !.
cmd_desc(move_focused_to_workspace(next_nonempty), "Move focused window to next workspace")     :- !.
cmd_desc(move_focused_to_workspace(N) , D) :- integer(N),compat_format(string(D), "Move focused window to workspace #~d", [N]), !.
cmd_desc(move_focused_to_workspace(W) , D) :-compat_format(string(D), "Move focused window to workspace ~p", [W]).
cmd_desc(switch_monitor(prev) , "Switch to previous monitor").
cmd_desc(switch_monitor(next) , "Switch to next monitor").
cmd_desc(switch_monitor(prev_nonempty), "Switch to previous non-empty monitor").
cmd_desc(switch_monitor(next_nonempty), "Switch to next non-empty monitor").
cmd_desc(switch_monitor(left) , "Switch monitor in left direction").
cmd_desc(switch_monitor(right), "Switch monitor in right direction").
cmd_desc(switch_monitor(up)   , "Switch monitor in up direction").
cmd_desc(switch_monitor(down) , "Switch monitor in down direction").
cmd_desc(switch_monitor(Mon) , D) :- string(Mon),compat_format(string(D), "Switch to monitor ~s", [Mon]).
cmd_desc(switch_monitor(Idx) , D) :- integer(Idx),compat_format(string(D), "Switch to monitor at index ~d", [Idx]).
cmd_desc(move_focused_to_monitor(prev) , "Move focused window to previous monitor").
cmd_desc(move_focused_to_monitor(next) , "Move focused window to next monitor").
cmd_desc(move_focused_to_monitor(prev_nonempty), "Move focused window to previous non-empty monitor").
cmd_desc(move_focused_to_monitor(next_nonempty), "Move focused window to next non-empty monitor").
cmd_desc(move_focused_to_monitor(left) , "Move focused window to monitor in left direction").
cmd_desc(move_focused_to_monitor(right), "Move focused window to monitor in right direction").
cmd_desc(move_focused_to_monitor(up)   , "Move focused window to monitor in up direction").
cmd_desc(move_focused_to_monitor(down) , "Move focused window to monitor in down direction").
cmd_desc(move_focused_to_monitor(Mon) , D) :- string(Mon),compat_format(string(D), "Move focused window to monitor ~s", [Mon]).
cmd_desc(move_focused_to_monitor(Idx) , D) :- integer(Idx),compat_format(string(D), "Move focused window to monitor at index ~d", [Idx]).
cmd_desc(menu:goto_window      , "Go to selected window, raise and focus it").
cmd_desc(menu:goto_workspace   , "Go to selected workspace").
cmd_desc(menu:pull_from        , "Pull selected window to active workspace").
cmd_desc(menu:push_to          , "Push focused window to the selected workspace").
cmd_desc(menu:close_windows    , "Close selected windows").
cmd_desc(menu:keep_windows     , "Close all windows other than the selected").
cmd_desc(menu:create_workspace , "Create new workspaces").
cmd_desc(menu:rename_workspace , "Rename selected workspace").
cmd_desc(menu:reindex_workspace, "Reindex selected workspace").
cmd_desc(menu:delete_workspaces, "Delete selected workspaces").
cmd_desc(menu:list_keymaps     , "List all defined keymaps").
cmd_desc(shellcmd              , "Run a shell command").
cmd_desc(reload_config         , "Reload configuration file").
cmd_desc(dump_settings         , "Dump current settings to a file").
cmd_desc(dump_changed_settings , "Dump settings that differ from the defaults to a file").
cmd_desc(shellcmd(Cmd), D) :-compat_format(string(D), "Run `~s`", [Cmd]).
cmd_desc(set(Setting), D) :- string_concat("Change setting ", Setting, D).
cmd_desc(add(Setting), D) :- string_concat("Add to setting ", Setting, D).

%! keybind_padded(++Keybind:[char], -PaddedKeybind:[char]) is det
%
%  Takes a list of characters of a key binding definition (see: keymaps/1)
%  and adds a ' ' (space) before and after each '+' in the list.
%
%  @arg Keybind original character list of key binding
%  @arg PaddedKeybind character list of key binding padded with spaces
keybind_padded([], []).
keybind_padded(['+'|Cs], [' ', '+', ' '|Rest]) :- !, keybind_padded(Cs, Rest).
keybind_padded([C|Cs], [C|Rest]) :- keybind_padded(Cs, Rest).

%! change_nmaster_prompt() is semidet
%
%  Prompts the user for a new nmaster value and if valid, sets nmaster to it.
%  Fails if the input is invalid.
change_nmaster_prompt :-
	(read_from_prompt("nmaster (+N, -N or N)", Input) ->
		ignore((catch(term_string(N, Input), Ex, (writeln(Ex), fail)), change_nmaster(N)))
	; true)
.

%! change_mfact_prompt() is semidet
%
%  Prompts the user for a new mfact value and if valid, sets mfact to it.
%  Fails if the input is invalid.
change_mfact_prompt :-
	(read_from_prompt("mfact (+F, -F or F)", Input) ->
		ignore((catch(term_string(F, Input), Ex, (writeln(Ex), fail)), change_mfact(F)))
	; true)
.

%! shellcmd_prompt() is det
%
%  Prompts the user for a shell command to run and executes it. See utils:shellcmd/1 for details.
shellcmd_prompt :-
	(read_from_prompt("shellcmd", Input) ->
		shellcmd(Input)
	; true)
.

%! change_setting_prompt(++SettingName:string, ++Append:bool) is semidet
%
%  Prompts the user for a new setting value and if valid, sets it.
%  Fails if the input is invalid.
%  If the Append flag is set, the setting (list) will be appended instead of overwritten.
%
%  @arg SettingName name of the setting to change
%  @arg Append whether it's a setting override or an appending (to a list typed setting)
change_setting_prompt(SettingName, Append) :-
	(Append = true -> PromptPrefix = "Add to " ; PromptPrefix = "Set "),
	string_concat(PromptPrefix, SettingName, PromptStr),

	(read_from_prompt(PromptStr, Input) ->
		atom_string(Setting, SettingName),
		ignore((
			catch(term_string(Value, Input), Ex, (writeln(Ex), fail)),
			(Append = true -> add(Setting, Value) ; set(Setting, Value))
		))
	; true)
.

%! dump_settings_prompt() is det
%
%  Prompts the user for a path to dump settings to. See dump_settings/2 for details.
dump_settings_prompt :-
	(read_from_prompt("Dump settings to", Input) ->
		dump_settings(Input, false)
	; true)
.

%! dump_changed_settings_prompt() is det
%
%  Prompts the user for a path to dump changed settings to. See dump_settings/2 for details.
dump_changed_settings_prompt :-
	(read_from_prompt("Dump changed settings to", Input) ->
		dump_settings(Input, true)
	; true)
.

%! run_cmd(++MenuEntries:[Action-Line], --Selection) is semidet
%
%  Should be passed as a callback to spawn_menu/3 with only the first argument filled.
%  All passed command entries are listed to the user, who can select one which gets executed.
%  Note: some commands will spawn additional prompt(s) to fill (see: read_from_prompt/2).
%
%  @arg MenuEntries command and line to display for that command in Action-Line format
%  @arg Selection list of selections - unused, filled be spawn_menu/3
run_cmd(MenuEntries, [Selection]) :-
	(member(Cmd-Selection, MenuEntries) ->
		(Cmd = set(Setting)          -> change_setting_prompt(Setting, false)
		;Cmd = add(Setting)          -> change_setting_prompt(Setting, true)
		;Cmd = change_nmaster        -> change_nmaster_prompt
		;Cmd = change_mfact          -> change_mfact_prompt
		;Cmd = shellcmd              -> shellcmd_prompt
		;Cmd = dump_settings         -> dump_settings_prompt
		;Cmd = dump_changed_settings -> dump_changed_settings_prompt
		;call(Cmd))
	; true)
.

%! list_keymaps() is det
%
%  List all defined key mappings from keymaps/1 in the format:
%    [Mod1 + ... + Modn +] Key      Command      Command description
%
%  If a selection happens, the selected mapping's Command is executed.
list_keymaps :-
	keymaps(Keymaps),
	findall(KBStr,
		(member((KB -> _), Keymaps),compat_format(chars(KBChars), "~p", [KB]),
		keybind_padded(KBChars, KBCharsPadded), string_chars(KBStr, KBCharsPadded)),
		KBStrs),
	findall(ActStr,
		(member((_ -> Act), Keymaps),compat_format(chars(ActStr), "~p", [Act])),
		ActStrs),
	maplist(string_length, KBStrs, KBWidths),
	maplist(string_length, ActStrs, ActWidths),
	max_list(KBWidths, KBMaxWidth),
	max_list(ActWidths, ActMaxWidth),
compat_format(string(Fmt), "~~s~~~d|~~p~~~d|~~s", [KBMaxWidth+3, KBMaxWidth+3+ActMaxWidth+3]),

	findall(Action-Line, (   % map key (Action) to lines for later lookup
		nth1(Idx, Keymaps, (_ -> Action)),
		nth1(Idx, KBStrs, KBStr),
		once((cmd_desc(Action, Desc) ; Desc = "")),
	compat_format(string(Line), Fmt, [KBStr, Action, Desc])),
		MenuEntries),
	findall(Line, member(_-Line, MenuEntries), Lines),
	spawn_menu("keymaps", Lines, menu:run_cmd(MenuEntries))
.

%! list_cmds() is det
%
%  List all defined user commands (i.e. predicate intended to be called by the user)
%  with their descriptions.
%  If a selection happens, the selected command is executed.
list_cmds :-
	findall(layout:set_layout(Layout), layout:is_layout(Layout), SLCmds1),
	delete(SLCmds1, layout:set_layout(nrows(_)), SLCmds2),
	delete(SLCmds2, layout:set_layout(ncols(_)), SLCmds3),
	findall(layout:set_layout(L), member(L, [nrows(2),nrows(3),nrows(4),ncols(2),ncols(3),ncols(4)]), SLCmds4),
	append(SLCmds3, SLCmds4, SetLayoutCmds),

	nb_getval(workspaces, Wss),
	findall(switch_workspace(Ws), member(Ws, Wss), SwitchWsByNameCmds),
	findall(switch_workspace(Idx), nth1(Idx, Wss, _), SwitchWsByIdxCmds),
	findall(move_focused_to_workspace(Ws), member(Ws, Wss), MoveToWsByNameCmds),
	findall(move_focused_to_workspace(Idx), nth1(Idx, Wss, _), MoveToWsByIdxCmds),

	monitors(Mons),
	findall(switch_monitor(Mon), member(Mon, Mons), SwitchMonCmds),
	findall(switch_monitor(Idx), nth1(Idx, Mons, _), SwitchMonByIdxCmds),
	findall(move_focused_to_monitor(Mon), member(Mon, Mons), MoveToMonCmds),
	findall(move_focused_to_monitor(Idx), nth1(Idx, Mons, _), MoveToMonByIdxCmds),

	append([
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
		SwitchMonByIdxCmds,
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
		MoveToMonByIdxCmds,
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
		menu:close_windows,
		menu:keep_windows,
		menu:create_workspace,
		menu:rename_workspace,
		menu:reindex_workspace,
		menu:delete_workspaces,
		menu:list_keymaps,
		shellcmd,
		reload_config,
		dump_settings,
		dump_changed_settings,
		set(default_nmaster),
		set(default_mfact),
		set(default_layout),
		set(attach_bottom),
		set(border_width),
		set(border_width_focused),
		set(border_color),
		set(border_color_focused),
		set(snap_threshold),
		set(outer_gaps),
		set(inner_gaps),
		set(workspaces),
		%set(starting_workspace), no sense to change, omit from the list
		set(hide_empty_workspaces),
		set(ws_format),
		set(ws_format_occupied),
		set(layout_default_overrides),
		set(bar_classes),
		set(bar_placement),
		%TODO set(fifo_enabled),
		%TODO set(fifo_path),
		set(menucmd),
		set(animation_enabled),
		set(animation_time),
		set(animation_granularity),
		set(modkey),
		set(scroll_up_action),
		set(scroll_down_action),
		set(keymaps),
		set(rules),
		set(hooks),
		%add(workspaces), we have menu:create_workspace/0 and create_workspace/1 for this
		add(layout_default_overrides),
		%add(menucmd), whole menu command should be changed at once from a user perspective
		add(keymaps),
		add(rules),
		add(hooks)
		]
	], Cmds),

	findall(CmdStr,
		(member(Cmd, Cmds),compat_format(chars(CmdStr), "~p", [Cmd])),
		CmdStrs),
	maplist(string_length, CmdStrs, CmdWidths),
	max_list(CmdWidths, CmdMaxWidth),
compat_format(string(Fmt), "~~p~~~d|~~s", [CmdMaxWidth+3]),

	findall(Cmd-Line, (   % map key (Cmd) to lines for later lookup
		member(Cmd, Cmds),
		cmd_desc(Cmd, Desc),
	compat_format(string(Line)), Fmt, [Cmd, Desc]),
		MenuEntries),
	findall(Line, member(_-Line, MenuEntries), Lines),
	spawn_menu("commands", Lines, menu:run_cmd(MenuEntries))
.
