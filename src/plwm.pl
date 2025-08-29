% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE
%
% plwm - an X11 window manager written in Prolog
%
% See docs/README.md to get started

version(0.5).

:- use_module(library(assoc)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(si)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(error)).

:- use_module(fifo).
:- use_module(layout).
:- use_module(menu).
:- use_module(setting).
:- use_module(utils).
:- use_module(xf86names).
:- use_module(plx).
:- use_module(stubs).

:- dynamic(display/1).          % Stores Display pointer returned by XOpenDisplay(3)
:- dynamic(screen/1).           % Stores Screen returned by DefaultScreen(3)
:- dynamic(rootwin/1).          % Stores Window returned by DefaultRootWindow(3)
:- dynamic(xrandr_available/0). % Fact if XRRQueryExtension(3) returns true
:- dynamic(config_flag/1).      % Custom config path specified with --config (if any)
:- dynamic(keymap_internal/3).  % Holds (KeyCode, ModMask, Action) triples parsed from keymaps/1
:- dynamic(wmatom/2).           % Holds interned WM atoms, e.g. query WM_STATE with wmatom(wmstate, WmState)
:- dynamic(netatom/2).          % Holds interned NET atoms, e.g. query _NET_WM_STATE with netatom(wmstate, NetWMState)
:- dynamic(hook/2).             % Holds (Event, Action) pairs parsed from hooks/1
:- dynamic(jobs/1).             % Holds pending list of command terms waiting for execution

% Configuration predicates
:- dynamic(default_nmaster/1).
:- dynamic(default_mfact/1).
:- dynamic(default_layout/1).
:- dynamic(attach_bottom/1).
:- dynamic(border_width/1).
:- dynamic(border_width_focused/1).
:- dynamic(border_color/1).
:- dynamic(border_color_focused/1).
:- dynamic(snap_threshold/1).
:- dynamic(outer_gaps/1).
:- dynamic(inner_gaps/1).
:- dynamic(workspaces/1).
:- dynamic(starting_workspace/1).
:- dynamic(hide_empty_workspaces/1).
:- dynamic(ws_format/1).
:- dynamic(ws_format_occupied/1).
:- dynamic(layout_default_overrides/1).
:- dynamic(bar_classes/1).
:- dynamic(bar_placement/1).
:- dynamic(fifo_enabled/1).
:- dynamic(fifo_path/1).
:- dynamic(menucmd/1).
:- dynamic(animation_enabled/1).
:- dynamic(animation_time/1).
:- dynamic(animation_granularity/1).
:- dynamic(modkey/1).
:- dynamic(scroll_up_action/1).
:- dynamic(scroll_down_action/1).
:- dynamic(keymaps/1).
:- dynamic(rules/1).
:- dynamic(hooks/1).

%*********************************  Globals  **********************************
%
% border_pixel           Allocated color for border of windows
% border_pixel_focused   Allocated color for border of focused windows
%
% active_mon             Active monitor
% workspaces             Workspace names (same as workspaces at the beginning)
% bars                   XIDs of windows considered bars, see: bar_classes/1
% dragged                XID and mouse parameters of window being dragged: [Subwin, Xroot, Yroot, Button] or none
% drag_initial_winattr   Initial attributes of dragged window: [X, Y, W, H]
%
% Association lists indexed by monitors (output names):
%   monitor_geom         Monitor geometry: [X, Y, W, H]
%   free_win_space       Monitor geometry minus space reserved for bars
%   active_ws            Active workspace
%   prev_ws              Previous workspace (this is where toggle_workspace/0 goes)
%
% Association lists indexed by monitor-workspace pairs,
%   nmaster              Number of master windows
%   mfact                Size factor between master and stack windows
%   layout               Currently applied layout
%   focused              XID of focused window or 0
%   windows              List of XIDs of windows on this workspace
%
% use the global_ predicates from utils.pl instead of the nb_ ones with these
%
% For all windows:                                  bool     4 integers
%   XID                  Window properties: [State, Fullscr, [X,Y,W,H]]
%                        State can be: managed, floating
%

%! quit is det
%
%  Terminates plwm by calling quit/1 with exit code 0 (no error).
quit :- quit(0).

%! quit(++ExitCode:integer) is det
%
%  Disconnects from X, closes the log output and terminates plwm with the
%  provided exit code.
%
%  @arg ExitCode the process should terminate with
quit(ExitCode) :-
	run_hook(quit),
	(display(Dp) -> plx:x_close_display(Dp) ; true),
	current_output(S), close(S),
	halt(ExitCode)
.

%! alloc_colors() is det
%
%  Allocates Xft colors, e.g. for colored window borders.
alloc_colors :-
	display(Dp), screen(Screen),

	border_color(BorderColor),
	border_color_focused(BorderColorFocused),

	plx:default_colormap(Dp, Screen, Colormap),
	plx:default_visual(Dp, Screen, Visual),
	plx:xft_color_alloc_name(Dp, Visual, Colormap, BorderColor,        BorderPixel),
	plx:xft_color_alloc_name(Dp, Visual, Colormap, BorderColorFocused, BorderPixelF),

	nb_setval(border_pixel,         BorderPixel),
	nb_setval(border_pixel_focused, BorderPixelF)
.

%! setup_wmatoms() is det
%
%  Initializes WM atoms (e.g. WM_STATE) for ICCCM compilance.
%
%  See: https://www.x.org/releases/X11R7.7/doc/xorg-docs/icccm/icccm.html
setup_wmatoms :-
	display(Dp),
	plx:x_intern_atom(Dp, "WM_STATE",         false, WmState),
	plx:x_intern_atom(Dp, "WM_PROTOCOLS",     false, WmProtocols),
	plx:x_intern_atom(Dp, "WM_TAKE_FOCUS",    false, WmTakeFocus),
	plx:x_intern_atom(Dp, "WM_DELETE_WINDOW", false, WmDeleteWindow),

	assertz(wmatom(wmstate,        WmState)),
	assertz(wmatom(wmprotocols,    WmProtocols)),
	assertz(wmatom(wmtakefocus,    WmTakeFocus)),
	assertz(wmatom(wmdeletewindow, WmDeleteWindow))
.

%! setup_netatoms() is det
%
%  Initializes _NET atoms (e.g. _NET_NUMBER_OF_DESKTOPS) for EWMH compilance.
%
%  See: https://specifications.freedesktop.org/wm-spec/latest/
%
%  Static atoms like _NET_WM_NAME will be set once.
%  Dynamic atoms like _NET_CLIENT_LIST are asserted as dynamic predicates for later use.
setup_netatoms :-
	display(Dp), rootwin(Rootwin),
	plx:x_intern_atom(Dp, "UTF8_STRING"             , false, Utf8string),
	plx:x_intern_atom(Dp, "_NET_SUPPORTED"          , false, NetSupported),
	plx:x_intern_atom(Dp, "_NET_CLIENT_LIST"        , false, NetClientList),
	plx:x_intern_atom(Dp, "_NET_NUMBER_OF_DESKTOPS" , false, NetNumberOfDesktops),
	plx:x_intern_atom(Dp, "_NET_DESKTOP_GEOMETRY"   , false, NetDesktopGeometry),
	plx:x_intern_atom(Dp, "_NET_DESKTOP_VIEWPORT"   , false, NetDesktopViewport),
	plx:x_intern_atom(Dp, "_NET_CURRENT_DESKTOP"    , false, NetCurrentDesktop),
	plx:x_intern_atom(Dp, "_NET_DESKTOP_NAMES"      , false, NetDesktopNames),
	plx:x_intern_atom(Dp, "_NET_ACTIVE_WINDOW"      , false, NetActiveWindow),
	plx:x_intern_atom(Dp, "_NET_WORKAREA"           , false, NetWorkArea),
	plx:x_intern_atom(Dp, "_NET_SUPPORTING_WM_CHECK", false, NetSupportingWMCheck),
	plx:x_intern_atom(Dp, "_NET_WM_NAME"            , false, NetWMName),

	plx:x_intern_atom(Dp, "_NET_WM_WINDOW_TYPE"       , false, NetWMWindowType),
	plx:x_intern_atom(Dp, "_NET_WM_WINDOW_TYPE_DIALOG", false, NetWMWindowTypeDialog),

	plx:x_intern_atom(Dp, "_NET_WM_STATE"             , false, NetWMState),
	plx:x_intern_atom(Dp, "_NET_WM_STATE_FULLSCREEN"  , false, NetWMStateFullscreen),

	XA_ATOM is 4, XA_CARDINAL is 6, XA_WINDOW is 33, PropModeReplace is 0,
	NetAtoms = [NetSupported, NetClientList, NetNumberOfDesktops, NetDesktopGeometry,
	            NetDesktopViewport, NetCurrentDesktop, NetDesktopNames, NetActiveWindow,
	            NetWorkArea, NetSupportingWMCheck, NetWMName],
	nb_getval(workspaces, Wss),
	nb_getval(active_mon, ActMon), global_key_value(monitor_geom, ActMon, [MX, MY, MW, MH]),
	global_key_value(active_ws, ActMon, ActWs), nth0(ActWsIdx, Wss, ActWs),

	plx:x_change_property(Dp, Rootwin, NetSupported       , XA_ATOM    , 32, PropModeReplace, NetAtoms,   11),
	plx:x_change_property(Dp, Rootwin, NetDesktopGeometry , XA_CARDINAL, 32, PropModeReplace, [MW, MH],   2),
	plx:x_change_property(Dp, Rootwin, NetDesktopViewport , XA_CARDINAL, 32, PropModeReplace, [MX, MY],   2),
	plx:x_change_property(Dp, Rootwin, NetCurrentDesktop  , XA_CARDINAL, 32, PropModeReplace, [ActWsIdx], 1),

	plx:x_create_simple_window(Dp, Rootwin, 0, 0, 1, 1, 0, 0, 0, WMCheckWin),
	plx:x_change_property(Dp, WMCheckWin, NetSupportingWMCheck, XA_WINDOW , 32, PropModeReplace, [WMCheckWin], 1),
	plx:x_change_property(Dp, WMCheckWin, NetWMName           , Utf8string,  8, PropModeReplace, "plwm"      , 4),
	plx:x_change_property(Dp, Rootwin   , NetSupportingWMCheck, XA_WINDOW , 32, PropModeReplace, [WMCheckWin], 1),

	% These will be changed dynamically
	assertz(netatom(clientlist,          NetClientList)),
	assertz(netatom(numberofdesktops,    NetNumberOfDesktops)),
	assertz(netatom(currentdesktop,      NetCurrentDesktop)),
	assertz(netatom(desktopnames,        NetDesktopNames)),
	assertz(netatom(activewindow,        NetActiveWindow)),
	assertz(netatom(workarea,            NetWorkArea)),
	assertz(netatom(wmwindowtype,        NetWMWindowType)),
	assertz(netatom(wmwindowtype_dialog, NetWMWindowTypeDialog)),
	assertz(netatom(wmstate,             NetWMState)),
	assertz(netatom(wmstatefullscreen,   NetWMStateFullscreen))
.

%! modifier(++Mod:atom) is det
%
%  Checks whether the provided key is a modifier key.
%
%  @arg Mod modifier key
modifier(Mod) :- member(Mod, [shift, lock, ctrl, alt, mod2, mod3, super, mod5]).

%! modkey_mask_newmask(++ModKey:atom, ++Mask:integer, -NewMask:integer) is det
%
%  Sets a bit in a modifier mask according to the specified modifier key.
%
%  @arg ModKey modifier key (ctrl, shift, etc.)
%  @arg Mask input modifier mask (X11)
%  @arg NewMask output modifier mask (X11) equal to Mask + ModKey
modkey_mask_newmask(ModKey, Mask, NewMask) :-
	(
	ModKey = shift -> KeyMask is 1 << 0 ;
	ModKey = lock  -> KeyMask is 1 << 1 ;
	ModKey = ctrl  -> KeyMask is 1 << 2 ;
	ModKey = alt   -> KeyMask is 1 << 3 ;
	ModKey = mod2  -> KeyMask is 1 << 4 ;
	ModKey = mod3  -> KeyMask is 1 << 5 ;
	ModKey = super -> KeyMask is 1 << 6 ;
	ModKey = mod5  -> KeyMask is 1 << 7
	),
	NewMask is Mask \/ KeyMask
.

%! modkeys_mask(++ModKeys:[atom], -Mask:integer) is det
%
%  Constructs a modifier key mask for X11 from a list of modifier keys by setting
%  the appropriate bit flags.
%
%  @arg ModKeys list of modifier keys (ctrl, shift, etc.)
%  @arg Mask modifier key mask (X11)
modkeys_mask(ModKeys, Mask) :-
	foldl(modkey_mask_newmask, ModKeys, 0, Mask)
.

%! translate_keymap(++Key:string, ++Mods:[atom], :Action:callable) is semidet
%
%  Tries interpreting the provided key with various methods: XStringToKeysym(3),
%  XF86keysym mapping, direct key code.
%  Constructs the modifier mask from the list of modifier keys.
%  If either of the above two steps fails, the predicate fails, otherwise:
%  registers the specified action predicate to the key + modifiers button combination.
%
%  @arg Key string representation of an X11 key
%  @arg ModKeys list of modifier keys (ctrl, shift, etc.)
%  @arg Action predicate registered for invocation when Key + Mods are pressed
translate_keymap(Key, Mods, Action) :-
	plx:x_string_to_keysym(Key, Ksym1),
	(Ksym1 =\= 0 -> Ksym is Ksym1       % try with XStringToKeysym()
	; xf86names:name_ksym(Key, Ksym)    % try lookup from xf86names table
	; number_string(Ksym, Key)),        % try interpreting Key as the Ksym itself

	display(Dp),
	plx:x_keysym_to_keycode(Dp, Ksym, Kcode),
	(Kcode =\= 0 ->
		modkeys_mask(Mods, ModMask),
		ignore(retractall(keymap_internal(Kcode, ModMask, _))), % allow overwriting with new action
		(Action \= none ->
			assertz(keymap_internal(Kcode, ModMask, Action))
		; true)
	)
.

%! keybind_to_keylist(++Keybind:term, -KeyList:[term]) is det
%
%  Translates from the configuration key binding format to a list of keys.
%  E.g.
%    super + "Return"    -->  [super, "Return"]
%    super + shift + 1   -->  [super, shift, 1]
%    "AudioRaiseVolume"  -->  ["AudioRaiseVolume"]
%  Keys can be atoms (most usual), strings (e.g. key names with uppercase letters)
%  or integers (number buttons).
%
%  @arg Keybind compound term representing list of keys in forms A, A+B, A+B+C,...
%  @arg KeyList list of keys
keybind_to_keylist(A, [A]) :- atom(A) ; chars_si(A) ; integer(A).
keybind_to_keylist(L + R, List) :-
	keybind_to_keylist(L, LL),
	keybind_to_keylist(R, RL),
	append(LL, RL, List)
.

%! grab_keys() is det
%
%  Registers all key mappings defined by keymaps/1. Produces a warning
%  message for any invalid mapping.
%  For all successfully registered mappings, grabs its keys with XGrabKey(3),
%  i.e. these key combinations will be listened to and handled in XNextEvent(3).
grab_keys :-
	display(Dp), rootwin(Rootwin), GrabModeAsync is 1, AnyKey is 0, AnyModifier is 1 << 15,

	% ungrab first, because grab_keys/0 can be re-run when the keymaps/1 setting gets changed
	retractall(keymap_internal(_, _, _)),
	plx:x_ungrab_key(Dp, AnyKey, AnyModifier, Rootwin),

	keymaps(Keymaps),
	forall(member((Keybind -> Action), Keymaps), (
		keybind_to_keylist(Keybind, KeyList), length(KeyList, N), Nm1 is N - 1,
			utils:split_at(Nm1, KeyList, Mods, [Key]),
		atom_string(Key, KeyStr), maplist(atom_string, ModAtoms, Mods),
			(\+ translate_keymap(KeyStr, ModAtoms, Action) ->
				format(user_error, "warning: invalid key: ~q in keymap, ignored~n", [Key])
		; true)
	)),
	forall(keymap_internal(Kcode, ModMask, _),
		plx:x_grab_key(Dp, Kcode, ModMask, Rootwin, true, GrabModeAsync, GrabModeAsync)
	)
.

%! grab_buttons() is det
%
%  Grabs the left and right mouse buttons with modifier modkey/1 using
%  XGrabButton(3) to handle mouse events (e.g. window movement, resizing,
%  focus by hover) in XNextEvent(3).
%  Also grabs the scroll button to support running custom logic on scroll
grab_buttons :-
	display(Dp), rootwin(Rootwin),
	modkey(ModKey),
	modkey_mask_newmask(ModKey, 0, ModKeyVal),
	GrabModeAsync is 1,
	Button1 is 1, Button3 is 3, % left and right mouse buttons
	Button4 is 4, Button5 is 5, % up and down mouse scroll
	AnyButton is 0, AnyModifier is 1 << 15,
	ButtonPressMask   is 1 << 2,
	ButtonReleaseMask is 1 << 3,
	PointerMotionMask is 1 << 6,
	ButtonMask is ButtonPressMask \/ ButtonReleaseMask \/ PointerMotionMask,

	% ungrab first, because grab_buttons/0 can be re-run when the modkey/1 setting gets changed
	plx:x_ungrab_button(Dp, AnyButton, AnyModifier, Rootwin),

	plx:x_grab_button(Dp, Button1, ModKeyVal, Rootwin, true, ButtonMask, GrabModeAsync, GrabModeAsync, 0, 0),
	plx:x_grab_button(Dp, Button3, ModKeyVal, Rootwin, true, ButtonMask, GrabModeAsync, GrabModeAsync, 0, 0),

	% mouse scroll
	plx:x_grab_button(Dp, Button4, ModKeyVal, Rootwin, true, ButtonPressMask, GrabModeAsync, GrabModeAsync, 0, 0),
	plx:x_grab_button(Dp, Button5, ModKeyVal, Rootwin, true, ButtonPressMask, GrabModeAsync, GrabModeAsync, 0, 0)
.

%! setup_root_win() is det
%
%  Constructs the event mask for all X11 events the wm uses (e.g. ButtonPress, PointerMotion).
%  The mask is then used to initialize X using XChangeWindowAttributes(3) and XSelectInput(3).
%  Also initializes the default mouse cursor.
setup_root_win :-
	CWEventMask is 1 << 11,
	ButtonPressMask          is 1 << 2,
	EnterWindowMask          is 1 << 4,
	LeaveWindowMask          is 1 << 5,
	PointerMotionMask        is 1 << 6,
	StructureNotifyMask      is 1 << 17,
	SubstructureNotifyMask   is 1 << 19,
	SubstructureRedirectMask is 1 << 20,
	PropertyChangeMask       is 1 << 22,

	EventMask is ButtonPressMask \/ EnterWindowMask \/ LeaveWindowMask \/ PointerMotionMask \/
	             StructureNotifyMask \/ SubstructureNotifyMask \/ SubstructureRedirectMask \/ PropertyChangeMask,

	display(Dp), rootwin(Rootwin),

	plx:x_change_window_attributes(Dp, Rootwin, CWEventMask, EventMask),
	plx:x_select_input(Dp, Rootwin, EventMask),

	XC_left_ptr is 68,
	plx:x_create_font_cursor(Dp, XC_left_ptr, Cursor),
	plx:x_define_cursor(Dp, Rootwin, Cursor)
.

%! set_border(++Win:integer) is det
%
%  Sets the border of the specified window.
%  For focused windows, border_width_focused/1 implies the width,
%  otherwise border_width/1 is used.
%  Fullscreen windows are borderless.
%
%  @arg Win XID of window to set the border for
set_border(Win) :-
	display(Dp), CWBorderWidth is 1 << 4,

	(global_value(focused, Win) ->
		nb_getval(border_pixel_focused, BorderPixel),
		border_width_focused(BorderW), CWBorderWidth is 1 << 4
	;
		nb_getval(border_pixel, BorderPixel),
		border_width(BorderW), CWBorderWidth is 1 << 4
	),
	% is in fullscreen?
	(win_properties(Win, [_, true, _]) -> ActualBorderW is 0 ; ActualBorderW is BorderW),

	plx:x_set_window_border(Dp, Win, BorderPixel),
	plx:x_configure_window(Dp, Win, CWBorderWidth, 0, 0, 0, 0, ActualBorderW, 0, 0)
.

%! focus(++Win:integer) is det
%
%  Focuses the specified window. This entails a monitor switch (if necessary),
%  unfocusing the previous window (if any), giving input focus to the new window,
%  applying the focused border width and color and setting the ActiveWindow netatom.
%
%  @arg Win XID of window to receive focus
focus(Win) :-
	(Win =\= 0 ->
		display(Dp),
		win_mon_ws(Win, Mon, Ws),
		switch_monitor(Mon),
		unfocus_at_onlyvisual(Mon-Ws, false),

		RevertToPointerRoot is 1, CurrentTime is 0,
		plx:x_set_input_focus(Dp, Win, RevertToPointerRoot, CurrentTime),

		active_mon_ws(_, ActWs),
		(Ws == ActWs ->
			rootwin(Rootwin), netatom(activewindow, NetActiveWindow),
			XA_WINDOW is 33, PropModeReplace is 0,
			plx:x_change_property(Dp, Rootwin, NetActiveWindow, XA_WINDOW, 32, PropModeReplace, [Win], 1),

			wmatom(wmtakefocus, WMTakeFocus),
			ignore(send_event(Win, WMTakeFocus)) % window may not implement WM_TAKE_FOCUS
		; true),
		global_key_newvalue(focused, Mon-Ws, Win),
		set_border(Win)
	; true)
.

%! unfocus_onlyvisual(++OnlyVisual:bool) is det
%
%  Calls unfocus_at_onlyvisual/2 on the active monitor-workspace, OnlyVisual argument is forwarded.
%  See description of unfocus_at_onlyvisual/2 for more details.
%
%  @arg OnlyVisual whether to unfocus only visually
unfocus_onlyvisual(OnlyVisual) :- active_mon_ws(ActMon, ActWs), unfocus_at_onlyvisual(ActMon-ActWs, OnlyVisual).

%! unfocus_at_onlyvisual(++At:integer-atom, ++OnlyVisual:bool) is det
%
%  Unfocuses the active window on the target monitor-workspace.
%  This entails removing the focused status, adjusting the ActiveWindow netatom
%  and resetting the window border.
%
%  If OnlyVisual is true, focus is only dropped visually without taking away the focused status.
%  This is necessary when using multiple monitors: only a single window should be
%  highlighted as focused at a time, but we must remember which window is the
%  focused one when switching back to the previous monitor.
%
%  @arg At location of the unfocus in Monitor-Workspace (integer-atom) format
%  @arg OnlyVisual whether to unfocus only visually
unfocus_at_onlyvisual(Mon-Ws, OnlyVisual) :-
	global_key_value(focused, Mon-Ws, FocusedWin),
	(FocusedWin =\= 0 ->
		display(Dp), rootwin(Rootwin),
		netatom(activewindow, NetActiveWindow),
		plx:x_delete_property(Dp, Rootwin, NetActiveWindow),
		global_key_newvalue(focused, Mon-Ws, 0),
		set_border(FocusedWin),

		(OnlyVisual = true -> global_key_newvalue(focused, Mon-Ws, FocusedWin) ; true)
	; true)
.

%! shift_focus(++Direction:atom) is det
%
%  Shifts focus in the window stack in the specified direction (up or down).
%  Wrapping occurs if we reach the start/end of the stack.
%
%  @arg Direction down or up indicating which direction to shift the focus in
shift_focus(Direction) :-
	global_value(windows, Wins), global_value(focused, FocusedWin),
	length(Wins, WinCnt),
	(1 < WinCnt, FocusedWin =\= 0 -> (
		(Direction = down ->
			(nextto(FocusedWin, NextWin, Wins) -> focus(NextWin), raise(NextWin)
			; Wins = [FstWin|_], focus(FstWin), raise(FstWin))  % wrap to first
		; Direction = up ->
			(nextto(PrevWin, FocusedWin, Wins) -> focus(PrevWin), raise(PrevWin)
			; last(Wins, LastWin), focus(LastWin), raise(LastWin))  % wrap to last
		; utils:warn_invalid_arg("shift_focus", Direction)))
	; true)
.

%! raise(++Win:integer) is det
%
%  Raises the specified window, i.e. moves it to the top of the visual stack to be fully visible.
%
%  @arg Win XID of window to raise
raise(Win) :-
	display(Dp),
	CWStackMode is 1 << 6, Above is 0,
	plx:x_configure_window(Dp, Win, CWStackMode, 0, 0, 0, 0, 0, 0, Above)
.

%! hide(++Win:integer) is det
%
%  Hides the specified window by moving it outside of the visible screen space,
%  thus it won't be rendered until moved back with show/1.
%
%  @arg Win XID of window to hide
hide(Win) :-
	display(Dp),
	win_properties(Win, [_, _, [_, Y, W, H]]),
	OuterX is -2 * W - 1,
	plx:x_move_resize_window(Dp, Win, OuterX, Y, W, H) % move out of visible area
.

%! show(++Win:integer) is det
%
%  Un-hides the specified window previously hidden with hide/1 by moving it back
%  to the visible screen space.
%
%  @arg Win XID of window to show
show(Win) :-
	display(Dp),
	win_properties(Win, [_, _, [X, Y, W, H]]),
	plx:x_move_resize_window(Dp, Win, X, Y, W, H) % move back to visible area
.

%! close_window(++Win:integer) is det
%
%  Closes the specified window using XKillClient(3).
%
%  If the window implements the WM_DELETE_WINDOW protocol, then let it handle
%  the event itself by sending a ClientMessage instead.
%  E.g. this happens when Gimp only unmaps its Color Picker dialog to hide it.
%  Calling XKillClient() on it would kill the Gimp main window too.
%
%  @arg Win XID of window to close
close_window(Win) :-
	wmatom(wmdeletewindow, WmDeleteWindow),
	(Win =\= 0, \+ send_event(Win, WmDeleteWindow) ->
		display(Dp), DestroyAll is 0,

		plx:x_grab_server(Dp),
		plx:x_set_error_handler(true), % sets xerrordummy()
		plx:x_set_close_down_mode(Dp, DestroyAll),

		plx:x_kill_client(Dp, Win),

		plx:x_sync(Dp, false),
		plx:x_set_error_handler(false),
		plx:x_ungrab_server(Dp)
	; true)
.

%! close_focused() is det
%
%  Closes the currently focused window with close_window/1.
close_focused :-
	global_value(focused, FocusedWin),
	close_window(FocusedWin)
.

%! win_fullscreen(++Win:integer, ++Fullscr:bool) is det
%
%  Sets fullscreen status of the specified window.
%  Fullscreen windows don't adhere to free_win_space, i.e. they cover the full
%  screen space (including that of bars). Also, they are borderless.
%
%  @arg Win XID of window to set fullscreen status for
%  @arg Fullscr fullscreen flag: true or false
win_fullscreen(Win, Fullscr) :-
	(win_properties(Win, [State, _, Geom]) ->
		win_newproperties(Win, [State, Fullscr, Geom]),
		set_border(Win),
		layout:relayout
	; true)
.

%! toggle_floating() is det
%
%  Toggles the floating status of the focused window.
%  Floating = unmanaged, i.e. does not follow the tiling layout.
toggle_floating :-
	global_value(focused, FocusedWin),
	(FocusedWin =\= 0 ->
		win_properties(FocusedWin, [OldState|Rest]),
		(OldState = managed -> NewState = floating ; NewState = managed),
		win_newproperties(FocusedWin, [NewState|Rest]),
		layout:relayout
	; true)
.

%! toggle_fullscreen() is det
%
%  Toggles the fullscreen status of the focused window with win_fullscreen/2.
toggle_fullscreen :-
	global_value(focused, FocusedWin),
	(FocusedWin =\= 0 ->
		win_properties(FocusedWin, [_, Fullscr, _]),
		utils:bool_negated(Fullscr, NFullscr),
		win_fullscreen(FocusedWin, NFullscr)
	; true)
.

%! toggle_workspace() is det
%
%  Switches between the current and the previously active (if any) workspace.
toggle_workspace :-
	nb_getval(active_mon, ActMon), global_key_value(prev_ws, ActMon, PrevWs),
	switch_workspace(PrevWs)
.

%! toggle_hide_empty_workspaces() is det
%
%  Toggles between hiding and showing empty workspaces.
toggle_hide_empty_workspaces :-
	hide_empty_workspaces(State),
	utils:bool_negated(State, NState),
	set(hide_empty_workspaces, NState)
.

%! active_mon_ws(-ActMon:string, -ActWs:atom) is det
%
%  Returns the active monitor and workspace name.
active_mon_ws(ActMon, ActWs) :- nb_getval(active_mon, ActMon), global_key_value(active_ws, ActMon, ActWs).

%! monitors(-Mons:[string]) is det
%
%  Returns the list of output names of the monitors plwm manages.
monitors(Mons) :- nb_getval(active_ws, Assoc), assoc_to_keys(Assoc, Mons).

%! monws_keys(-MonWsKeys:[string-atom]) is det
%
%  Returns the list of all Monitor-Workspace combinations.
%
%  Note: we could query this from an assoc indexed by Mon-Ws, but since the introduction of
%  dynamic workspaces, the ordering gets inconsistent in the assoces, that's why we need this.
monws_keys(MonWsKeys) :-
	monitors(Mons), nb_getval(workspaces, Wss),
	findall(Mon-Ws, (member(Mon, Mons), member(Ws, Wss)), MonWsKeys)
.

%! selector_workspace(++Selector:term, -Ws:atom) is det
%
%  Selects a workspace based on the specified selector expression:
%  - concrete workspace name => that workspace
%  - workspace index => workspace at that index, or the active one if index is invalid
%  - prev/next => previous/next to the active workspace (wraps at the front/end of the list)
%  - prev_nonempty/next_nonempty => like prev/next, but only considering the non-empty workspaces
%
%  @arg Selector can be workspace name (atom), an index, prev, next, prev_nonempty, next_nonempty
%  @arg Ws workspace name (atom) the Selector points to
selector_workspace(Ws, Ws) :- nb_getval(workspaces, Wss), member(Ws, Wss), !.
selector_workspace(Selector, Ws) :-
	nb_getval(workspaces, Wss), length(Wss, WsCnt), active_mon_ws(_, ActWs),
	(integer(Selector) ->  % lookup by ws index, or fallback to active one if index is invalid
		(between(1, WsCnt, Selector) -> nth1(Selector, Wss, Ws) ; Ws = ActWs)
	;
	((Selector = prev_nonempty ; Selector = next_nonempty) -> nonempty_workspaces_and_act(WssToUse) ; WssToUse = Wss),
	((Selector = prev ; Selector = prev_nonempty) -> % wraps to last/first
		(nextto(PrevWs, ActWs, WssToUse) -> Ws = PrevWs
		; last(WssToUse, LastWs),           Ws = LastWs)
	;(Selector = next ; Selector = next_nonempty) ->
		(nextto(ActWs, NextWs, WssToUse) -> Ws = NextWs
		; WssToUse = [FstWs|_],             Ws = FstWs)
	))
.

%! compare_mongeom(++Dim:atom, -Order:term, ++MonGeom1:term, ++MonGeom2:term) is det
%
%  Compares two monitor geometries based on the specified x or y dimension.
%  Works like the standard compare/3 predicate, i.e. it returns ordering <, > or =.
%
%  @arg Dim dimension the comparison must be based on: x or y
%  @arg Order result of comparison returned by the standard compare/3: <, > or =
%  @arg MonGeom1 left monitor-geometry pair in form M1-[X1,Y1,W1,H1]
%  @arg MonGeom2 right monitor-geometry pair in form M2-[X2,Y2,W2,H2]
compare_mongeom(x, Order, _-[X1  |_], _-[X2  |_]) :- compare(Order, X1, X2).
compare_mongeom(y, Order, _-[_,Y1|_], _-[_,Y2|_]) :- compare(Order, Y1, Y2).

%! selector_monitor(++Selector:term, -Mon:string) is det
%
%  Selects a monitor based on the specified selector expression:
%  - concrete monitor name => that monitor
%  - monitor index => monitor on that index
%  - left/right/up/down => monitor relative to the active one (no wrapping)
%  - prev/next => previous/next to the active monitor (wraps at the front/end of the list)
%  - prev_nonempty/next_nonempty => like prev/next, but only considering monitors with displayed windows
%
%  @arg Selector a monitor name (string) or index, left, right, up, down, prev, next, prev_nonempty, next_nonempty
%  @arg Mon monitor name the Selector points to
selector_monitor(Selector, Mon) :-
	monitors(Mons), nb_getval(active_mon, ActMon),
	(member(Selector, Mons) ->  % we may pass the Mon name itself
		Mon = Selector
	; integer(Selector) ->
		nth1(Selector, Mons, Mon)
	; member(Selector, [left, right, up, down]) ->  % lookup by direction from active mon
		nb_getval(monitor_geom, AMonGeom), assoc_to_list(AMonGeom, MonGeomPairs),
		((Selector = left ; Selector = right) -> predsort(compare_mongeom(x), MonGeomPairs, SortedPairs)
		;                                        predsort(compare_mongeom(y), MonGeomPairs, SortedPairs)),
		pairs_keys(SortedPairs, SortedKeys),
		((Selector = left ; Selector = down) -> nextto(Mon, ActMon, SortedKeys)
		;                                       nextto(ActMon, Mon, SortedKeys))
	;
	((Selector = prev_nonempty ; Selector = next_nonempty) -> nonempty_monitors(MonsToUse) ; MonsToUse = Mons),
	((Selector = prev ; Selector = prev_nonempty) -> % wraps to last/first
		(nextto(PrevMon, ActMon, MonsToUse) -> Mon = PrevMon
		; last(MonsToUse, LastMon),            Mon = LastMon)
	;(Selector = next ; Selector = next_nonempty) ->
		(nextto(ActMon, NextMon, MonsToUse) -> Mon = NextMon
		; MonsToUse = [FstMon|_],              Mon = FstMon)
	))
.

%! switch_workspace(++Selector:term) is det
%
%  Switches workspace on the active monitor to the one specified by a selector
%  using switch_workspace/2.
%
%  @arg Selector can be workspace name (atom), an index, prev, next, prev_nonempty, next_nonempty
switch_workspace(Selector) :- active_mon_ws(ActMon, _), switch_workspace(ActMon, Selector).

%! switch_workspace(++Mon:string, ++Selector:term) is det
%
%  Switches workspace on a given monitor to the one specified by a selector.
%
%  @arg Mon monitor name to switch workspace on
%  @arg Selector can be workspace name (atom), an index, prev, next, prev_nonempty, next_nonempty
switch_workspace(Mon, Selector) :-
	global_key_value(active_ws, Mon, OldWs),
	(selector_workspace(Selector, NewWs) ->
		(OldWs \= NewWs ->
			run_hook(switch_workspace_pre),

			display(Dp), rootwin(Rootwin),
			global_key_newvalue(active_ws, Mon, NewWs), global_key_newvalue(prev_ws, Mon, OldWs),
			global_key_value(windows, Mon-NewWs, NewWins),
			global_key_value(windows, Mon-OldWs, OldWins),

			forall(member(Win, OldWins), hide(Win)),
			forall(member(Win, NewWins), show(Win)),
			layout:relayout,

			% focused window was remembered, restore it (if any)
			global_value(focused, FocusedWin),
			(FocusedWin =\= 0 ->
				focus(FocusedWin)
			;
				netatom(activewindow, NetActiveWindow),
				plx:x_delete_property(Dp, Rootwin, NetActiveWindow)
			),
			update_ws_atoms,

			run_hook(switch_workspace_post)
		; true)
	; utils:warn_invalid_arg("switch_workspace", Selector))
.

%! shiftcoord_win_from_to(++Win:integer, ++FromMon:string, ++ToMon:string) is det
%
%  Shifts the screen coordinates of the specified window from a monitor to a new one.
%  E.g. used when a floating window is moved from one monitor to another.
%
%  @arg Win XID of window to shift the coordinate of
%  @arg FromMon origin monitor of the window
%  @arg ToMon target monitor of the window
shiftcoord_win_from_to(Win, FromMon, ToMon) :-
	(FromMon \= ToMon ->
		(nb_getval(bars, Bars), \+ member(Win, Bars) -> show(Win) ; true),
		display(Dp),
		global_key_value(monitor_geom, FromMon, [FromMonX, FromMonY|_]),
		global_key_value(monitor_geom, ToMon,   [ToMonX,   ToMonY  |_]),
		(plx:x_get_window_attributes(Dp, Win, [X, Y, W, H], Status), Status =\= 0 ->
			MonDiffX is FromMonX - ToMonX, NewX is X - MonDiffX,
			MonDiffY is FromMonY - ToMonY, NewY is Y - MonDiffY,
			plx:x_move_resize_window(Dp, Win, NewX, NewY, W, H),
			(nb_getval(bars, Bars), \+ member(Win, Bars) ->
				win_properties(Win, [State, Fullscr, _]),
				win_newproperties(Win, [State, Fullscr, [NewX, NewY, W, H]]),

				global_key_value(active_ws, ToMon, ActWs), % re-hide (coord shift)
				win_mon_ws(Win, ToMon, WsOfWin),           % if needed
				(WsOfWin \= ActWs -> hide(Win) ; true)
			; true)
		; true)
	; true)
.

%! switch_monitor(++To:term) is det
%
%  Switches focus from the active monitor to a new one.
%
%  @arg To can be a monitor name (string), left, right, up, down, prev, next, prev_nonempty, next_nonempty
%       See selector_monitor/2 for more information about the values
switch_monitor(To) :-
	active_mon_ws(ActMon, ActWs),
	(selector_monitor(To, ToMon) ->
		(ToMon \= ActMon ->
			run_hook(switch_monitor_pre),

			unfocus_at_onlyvisual(ActMon-ActWs, true), % switch focus
			nb_setval(active_mon, ToMon), active_mon_ws(ToMon, NewActWs),
			global_key_value(focused, ToMon-NewActWs, FocusedWin), focus(FocusedWin),

			(bar_placement(follow_focus) ->
				nb_getval(bars, Bars),
				forall(member(Bar, Bars), shiftcoord_win_from_to(Bar, ActMon, ToMon))
				% move the bar(s) to the newly active monitor
			; true),
			update_ws_atoms,
			update_workarea,

			run_hook(switch_monitor_post)
		; true)
	; utils:warn_invalid_arg("switch_monitor", To))
.

%! win_tomon_toworkspace_top(++Win:integer, ++ToMon:string, ++ToWs:atom, ++Top:bool) is det
%
%  Moves the specified window to the top/bottom of the window stack on a target
%  monitor-workspace.
%
%  @arg Win XID of window to move
%  @arg ToMon target monitor of window movement
%  @arg ToWs target workspace of window movement
%  @arg Top if true, place Win to the top of the window stack, to the bottom otherwise
win_tomon_toworkspace_top(Win, ToMon, ToWs, Top) :-
	(Win =\= 0, win_mon_ws(Win, FromMon, FromWs), FromMon-FromWs \= ToMon-ToWs ->
		global_key_value(windows, FromMon-FromWs, FromWinsOld),
		global_key_value(windows, ToMon-ToWs,     ToWinsOld),

		once(nth0(Idx, FromWinsOld, Win, FromWinsNew)),
		(Top -> ToWinsNew = [Win|ToWinsOld] ; append(ToWinsOld, [Win], ToWinsNew)),
		global_key_newvalue(windows, FromMon-FromWs, FromWinsNew),
		global_key_newvalue(windows, ToMon-ToWs,     ToWinsNew),

		% unfocus on prev ws, focus on new, but don't take input
		unfocus_at_onlyvisual(FromMon-FromWs, false),
		focus(Win),
		display(Dp), RevertToPointerRoot is 1, CurrentTime is 0, PointerRoot is 1,
		plx:x_set_input_focus(Dp, PointerRoot, RevertToPointerRoot, CurrentTime),

		% focus next win on current ws, if any
		NextIdx is max(0, Idx - 1),
		(nth0(NextIdx, FromWinsNew, PrevWin) -> focus(PrevWin) ; true),

		(global_key_value(active_ws, FromMon, FromWs) ->
			hide(Win),
			layout:relayout(FromMon-FromWs)
		; true),
		(global_key_value(active_ws, ToMon, ToWs) ->
			layout:relayout(ToMon-ToWs)
		; true),

		% shift coords to new mon for floating (or about to be floated) wins
		win_properties(Win, [State|_]),
		(FromMon \= ToMon, (global_key_value(layout, ToMon-ToWs, floating) ; State \= managed) ->
			shiftcoord_win_from_to(Win, FromMon, ToMon)
		; true),

		update_ws_atoms
	; true)
.

%! move_focused_to_workspace(++Selector:term) is det
%
%  Moves the focused window to the specified workspace on the active monitor.
%
%  @arg Selector can be workspace name (atom), an index, prev, next, prev_nonempty, next_nonempty
%       See selector_workspace/2 for more information about the values
move_focused_to_workspace(Selector) :-
	global_value(focused, FocusedWin), nb_getval(active_mon, ActMon),
	(selector_workspace(Selector, ToWs) ->
		win_tomon_toworkspace_top(FocusedWin, ActMon, ToWs, false)
	; utils:warn_invalid_arg("move_focused_to_workspace", Selector))
.

%! move_focused_to_monitor(++Selector:term) is det
%
%  Moves the focused window to the active workspace on the specified monitor.
%
%  @arg Selector can be a monitor name (string), left, right, up, down, prev, next, prev_nonempty, next_nonempty
%       See selector_monitor/2 for more information about the values
move_focused_to_monitor(Selector) :-
	active_mon_ws(ActMon, _), global_value(focused, FocusedWin),
	(selector_monitor(Selector, ToMon) ->
		(FocusedWin =\= 0, ToMon \= ActMon ->
			global_key_value(active_ws, ToMon, ToMonWs),
			win_tomon_toworkspace_top(FocusedWin, ToMon, ToMonWs, true),
			switch_monitor(ActMon) % focus should stay at source monitor
		; true)
	; utils:warn_invalid_arg("move_focused_to_monitor", Selector))
.

%! nonempty_workspaces_and_act(-NonEmptyWss:[atom]) is det
%
%  Returns the list of non-empty workspaces.
%  The active one is also included whether its empty or not.
%
%  @arg NonEmptyWss list of non-empty workspaces + the active one
nonempty_workspaces_and_act(NonEmptyWss) :- nonempty_workspaces(NonEmptyWss, true).

%! nonempty_workspaces(-NonEmptyWss:[atom]) is det
%
%  Returns the list of non-empty workspaces.
%
%  @arg NonEmptyWss list of non-empty workspaces
nonempty_workspaces(NonEmptyWss) :- nonempty_workspaces(NonEmptyWss, false).
nonempty_workspaces(NonEmptyWss, IncludeAct) :-
	nb_getval(workspaces, Wss), active_mon_ws(ActMon, ActWs),
	findall(Ws,
	        (member(Ws, Wss),
		once(((IncludeAct = true, Ws = ActWs) ; (global_key_value(windows, ActMon-Ws, [_|_]))))),
	        NonEmptyWss)
.

%! nonempty_monitors(-NonEmptyMons:[string]) is det
%
%  Returns the list of non-empty monitors.
%
%  @arg NonEmptyMons list of non-empty monitors
nonempty_monitors(NonEmptyMons) :- % also includes the active one, even if it's empty
	monitors(Mons), active_mon_ws(ActMon, _),
	findall(Mon,
	        (member(Mon, Mons), once((Mon = ActMon ;
	        (global_key_value(active_ws, Mon, ActWs), global_key_value(windows, Mon-ActWs, [_|_]))))),
	        NonEmptyMons)
.

%! create_workspace(++Ws:atom) is det
%
%  Creates a new workspace.
%  If the provided name is not unique, no operation takes place.
%  As per the multi-monitor concept, all monitors get an instance of the new workspace.
%
%  @arg Ws atom name of to be created workspace, must be unique
create_workspace(Ws) :-
	nb_getval(workspaces, Wss),
	(\+ member(Ws, Wss) -> % ws with this name must not already exist
		monitors(Mons),
		forall(member(Mon, Mons), (
			default_nmaster(Nmaster), default_mfact(Mfact), default_layout(Layout),
			nb_getval(nmaster, ANmaster), nb_getval(mfact, AMfact), nb_getval(layout, ALayout),
			nb_getval(focused, AFocused), nb_getval(windows, AWins),
			put_assoc(Mon-Ws, ANmaster, Nmaster, NewANmaster),
			put_assoc(Mon-Ws, AMfact,   Mfact,   NewAMfact),
			put_assoc(Mon-Ws, ALayout,  Layout,  NewALayout),
			put_assoc(Mon-Ws, AFocused, 0,       NewAFocused),
			put_assoc(Mon-Ws, AWins,    [],      NewAWins),
			nb_setval(nmaster, NewANmaster), nb_setval(mfact, NewAMfact), nb_setval(layout, NewALayout),
			nb_setval(focused, NewAFocused), nb_setval(windows, NewAWins),

			layout_default_overrides(LDefOverrides),
			forall(member((Mon, Ws -> NmasterOR, MfactOR, LayoutOR), LDefOverrides), (
				(ground(NmasterOR) -> global_key_newvalue(nmaster, Mon-Ws, NmasterOR) ; true),
				(ground(MfactOR)   -> global_key_newvalue(mfact,   Mon-Ws, MfactOR)   ; true),
				(ground(LayoutOR)  -> global_key_newvalue(layout,  Mon-Ws, LayoutOR)  ; true)
			))
		)),
		append(Wss, [Ws], NewWss),
		nb_setval(workspaces, NewWss),
		update_ws_atoms
	; true)
.

%! replace_value(++OldV:term, ++NewV:term, ++OldV:term, ++NewV:term) is det
%
%  Can be used as a meta-predicate. E.g. for map_assoc/3 to swap out a value with a new one,
%  i.e. replace_value(OldName, NewName) will instance it to a replace_value/2
%  predicate that swaps OldName with NewName.
%
%  @arg OldV old value
%  @arg NewV new value
replace_value(OldV, NewV, OldV, NewV) :- !.
replace_value(_, _, V, V).

%! replace_key(++Assoc:assoc, ++OldK:term, ++NewK:term, --NewAssoc:assoc) is det
%
%  Replaces a key in an association with a new one retaining its mapped value.
%
%  @arg Assoc original association
%  @arg OldK key to replace in the association
%  @arg NewK new key to replace the old with
%  @arg NewAssoc new association with the specified key replaced
replace_key(Assoc, OldK, NewK, NewAssoc) :-
	del_assoc(OldK, Assoc, Value, TrimmedAssoc),
	put_assoc(NewK, TrimmedAssoc, Value, NewAssoc)
.

%! rename_workspace(++OldName:atom, ++NewName:atom) is det
%
%  Gives a specified workspace a new name. The new name must be unique.
%
%  @arg OldName name of the workspace to be renamed
%  @arg NewName new name for the workspace
rename_workspace(OldName, NewName) :-
	nb_getval(workspaces, Wss),
	(\+ member(NewName, Wss) -> % names should be kept unique
		nb_getval(active_ws, AActiveWs), nb_getval(prev_ws, APrevWs),
		map_assoc(replace_value(OldName, NewName), AActiveWs, NewAActiveWs),
		map_assoc(replace_value(OldName, NewName), APrevWs,   NewAPrevWs),
		monitors(Mons),
		forall(member(Mon, Mons), (
			nb_getval(nmaster, ANmaster), nb_getval(mfact, AMfact), nb_getval(layout, ALayout),
			nb_getval(focused, AFocused), nb_getval(windows, AWins),
			replace_key(ANmaster, Mon-OldName, Mon-NewName, NewANmaster),
			replace_key(AMfact,   Mon-OldName, Mon-NewName, NewAMfact),
			replace_key(ALayout,  Mon-OldName, Mon-NewName, NewALayout),
			replace_key(AFocused, Mon-OldName, Mon-NewName, NewAFocused),
			replace_key(AWins,    Mon-OldName, Mon-NewName, NewAWins),
			nb_setval(nmaster, NewANmaster), nb_setval(mfact, NewAMfact), nb_setval(layout, NewALayout),
			nb_setval(focused, NewAFocused), nb_setval(windows, NewAWins)
		)),
		nb_setval(active_ws, NewAActiveWs), nb_setval(prev_ws, NewAPrevWs),
		select(OldName, Wss, NewName, NewWss),
		nb_setval(workspaces, NewWss),
		update_ws_atoms
	; true)
.

%! reindex_workspace(++Ws:atom, ++NewIdx:integer) is det
%
%  Moves the specified workspace to a new index position in the workspace list.
%
%  @arg Ws workspace to be reindexed
%  @arg NewIdx new index for the workspace
reindex_workspace(Ws, NewIdx) :-
	nb_getval(workspaces, Wss),
	selectchk(Ws, Wss, WssStripped),
	N is NewIdx - 1,
	utils:split_at(N, WssStripped, Prefix, Suffix),
	append(Prefix, [Ws], Prefix2),
	append(Prefix2, Suffix, NewWss),
	nb_setval(workspaces, NewWss),
	update_ws_atoms
.

%! delete_ws_assocs(++Mon:string, ++Ws:atom) is det
%
%  Deletes associations from memory related to a monitor-workspace.
%
%  @arg Mon monitor to delete the workspace associations from
%  @arg Ws workspace to delete the associations of
delete_ws_assocs(Mon, Ws) :-
	nb_getval(nmaster, ANmaster), nb_getval(mfact, AMfact), nb_getval(layout, ALayout),
	nb_getval(focused, AFocused), nb_getval(windows, AWins),
	del_assoc(Mon-Ws, ANmaster, _, NewANmaster),
	del_assoc(Mon-Ws, AMfact  , _, NewAMfact),
	del_assoc(Mon-Ws, ALayout , _, NewALayout),
	del_assoc(Mon-Ws, AFocused, _, NewAFocused),
	del_assoc(Mon-Ws, AWins   , _, NewAWins),
	nb_setval(nmaster, NewANmaster), nb_setval(mfact, NewAMfact), nb_setval(layout, NewALayout),
	nb_setval(focused, NewAFocused), nb_setval(windows, NewAWins)
.

%! delete_workspace(++Ws:atom) is det
%
%  Deletes the specified workspace.
%  If there were any windows on the workspace, they are moved to the next one (wraps).
%  If the last workspace would be deleted, no operation takes place.
%
%  @arg Ws workspace to delete
delete_workspace(Ws) :-
	monitors(Mons), nb_getval(workspaces, Wss),
	(Wss \= [_] ->  % don't allow deleting if there is only one ws left
		(nextto(Ws, NWs, Wss) -> NextWs = NWs ; Wss = [NextWs|_]),
		forall(member(Mon, Mons), (
			global_key_value(active_ws, Mon, ActWs),
			global_key_value(prev_ws, Mon, PrevWs),
			(Ws = ActWs -> switch_workspace(Mon, NextWs) ; true),
			(Ws = PrevWs -> global_key_newvalue(prev_ws, Mon, ActWs) ; true),

			global_key_value(windows, Mon-Ws, Wins),
			forall(member(Win, Wins), win_tomon_toworkspace_top(Win, Mon, NextWs, false)),

			delete_ws_assocs(Mon, Ws)
		)),
		selectchk(Ws, Wss, NewWss),
		nb_setval(workspaces, NewWss),
		update_ws_atoms
	; true)
.

%! delete_monitor(++Mon:string) is det
%
%  Deletes the specified monitor.
%  If there were any windows on workspaces of the monitor, they are moved to
%  the next monitor's respective workspaces (wraps).
%
%  Note: if the last monitor would be deleted, no operation takes place, so we
%  keep the logic even if no physical monitor is connected (subject to change).
%
%  @arg Mon Name of monitor to delete
delete_monitor(Mon) :-
	monitors(Mons),
	(Mons \= [_] ->  % keep logic of final monitor even if it's disconnected
		(nextto(Mon, NMon, Mons) -> NextMon = NMon ; Mons = [NextMon|_]),

		nb_getval(workspaces, Wss), % move windows to next monitor
		forall(member(Ws, Wss), (
			forall(global_key_value(windows, Mon-Ws, Wins), (
				forall(member(Win, Wins), win_tomon_toworkspace_top(Win, NextMon, Ws, false))
		)))),

		nb_getval(active_mon, ActMon), % focus next monitor if this was the focused one
		(ActMon == Mon -> switch_monitor(NextMon) ; true),

		nb_getval(monitor_geom, AMonGeom),  nb_getval(free_win_space, AFreeWinSpace),
		nb_getval(active_ws,    AActiveWs), nb_getval(prev_ws,        APrevWs),
		del_assoc(Mon, AMonGeom,  _, NewAMonGeom),  del_assoc(Mon, AFreeWinSpace, _, NewAFreeWinSpace),
		del_assoc(Mon, AActiveWs, _, NewAActiveWs), del_assoc(Mon, APrevWs,       _, NewAPrevWs),
		nb_setval(monitor_geom, NewAMonGeom),  nb_setval(free_win_space, NewAFreeWinSpace),
		nb_setval(active_ws,    NewAActiveWs), nb_setval(prev_ws,        NewAPrevWs),

		forall(member(Ws, Wss), delete_ws_assocs(Mon, Ws)),

		format("Monitor \"~s\" unmanaged~n", [Mon])
	; true)
.

%! format_ws_name(++Fmt:string, ++[Idx, Ws]:[integer, atom], -Formatted:atom) is det
%
%  Assembles a workspace name for display from a format string, workspace name
%  and optionally the workspace index.
%
%  @arg Fmt format string which must contain a ~w or a ~d followed by a ~w
%  @arg Idx workspace index
%  @arg Ws workspace name (atom)
%  @arg Formatted formatted output
format_ws_name(Fmt, [Idx, Ws], Formatted) :-
	(sub_string(Fmt, _, _, _, "~d") ->
			phrase(format_(Fmt, [Idx, Ws]), F)
		;	phrase(format_(Fmt, [Ws]     ), F)
	),
	atom_chars(Formatted, F)
.

%! update_ws_atoms() is det
%
%  Updates the workspace related netatoms (e.g. _NET_CURRENT_DESKTOP).
%    see: https://specifications.freedesktop.org/wm-spec/latest/
update_ws_atoms :-
	display(Dp), rootwin(Rootwin), active_mon_ws(_, ActWs), nb_getval(workspaces, Wss),
	netatom(numberofdesktops, NetNumberOfDesktops),
	netatom(desktopnames,     NetDesktopNames),
	netatom(currentdesktop,   NetCurrentDesktop),
	XUTF8StringStyle is 4, XA_CARDINAL is 6, PropModeReplace is 0,

	nonempty_workspaces_and_act(RelevantWss),
	nonempty_workspaces(NonEmptyWss),
	(hide_empty_workspaces(true) -> WssToUse = RelevantWss ; WssToUse = Wss),

	length(WssToUse, WsCnt),
	findall(DisplayedName,
		(nth1(Idx, Wss, Ws), selectchk(Ws, WssToUse, _),
		(selectchk(Ws, NonEmptyWss, _) ->
			ws_format_occupied(Fmt),
			format_ws_name(Fmt, [Idx, Ws], DisplayedName)
		;
			ws_format(Fmt),
			format_ws_name(Fmt, [Idx, Ws], DisplayedName)
		)),
		DisplayedNames
	),
	plx:x_utf8_text_list_to_text_property(Dp, DisplayedNames, WsCnt, XUTF8StringStyle, TextProp),
	plx:x_set_text_property(Dp, Rootwin, TextProp, NetDesktopNames),
	plx:x_change_property(Dp, Rootwin, NetNumberOfDesktops, XA_CARDINAL, 32, PropModeReplace, [WsCnt], 1),
	plx:c_free(TextProp),

	nth0(WsIdx, WssToUse, ActWs),  % this must always be updated
	plx:x_change_property(Dp, Rootwin, NetCurrentDesktop, XA_CARDINAL, 32, PropModeReplace, [WsIdx], 1)
.

%! update_workarea() is det
%
%  Updates the _NET_WORKAREA netatom.
%    see: https://specifications.freedesktop.org/wm-spec/latest/
update_workarea :-
	display(Dp), rootwin(Rootwin), nb_getval(active_mon, ActMon),
	global_key_value(free_win_space, ActMon, Geom),
	nb_getval(workspaces, Wss), length(Wss, WsCnt),

	utils:n_item_clones(WsCnt, Geom, Geoms), append(Geoms, Geoms1D),

	netatom(workarea, NetWorkArea),
	XA_CARDINAL is 6, PropModeReplace is 0, DataCnt is WsCnt * 4,
	plx:x_change_property(Dp, Rootwin, NetWorkArea, XA_CARDINAL, 32, PropModeReplace, Geoms1D, DataCnt)
.

%! update_wintype(++Win:integer) is det
%
%  Updates window state based on X11 protocol data:
%  - windows marked as dialogs (_NET_WM_WINDOW_TYPE_DIALOG) and transient ones
%    are automatically set to floating mode
%  - transient windows are moved to their parent's monitor-workspace
%  - windows with _NET_WM_STATE_FULLSCREEN are turned fullscreen
%    see: https://specifications.freedesktop.org/wm-spec/latest/
%
%  This predicate should be called for newly created windows as well as ones
%  that receive a PropertyNotify.
%
%  @arg Win XID of window to update the state for
update_wintype(Win) :-
	display(Dp),
	XA_ATOM is 4,
	netatom(wmwindowtype, NetWMWindowType), netatom(wmwindowtype_dialog, NetWMWindowTypeDialog),
	netatom(wmstate, NetWMState),           netatom(wmstatefullscreen, NetWMStateFullscreen),

	plx:x_get_window_property(Dp, Win, NetWMWindowType, false, XA_ATOM, TypeProperty),
	plx:x_get_window_property(Dp, Win, NetWMState,      false, XA_ATOM, StateProperty),
	plx:x_get_transient_for_hint(Dp, Win, TransientFor, TSStatus),

	(TSStatus = 0 -> ParentWin = 0 ; ParentWin = TransientFor),

	win_properties(Win, [State|Rest]),
	(State = managed, (TypeProperty == NetWMWindowTypeDialog ; ParentWin =\= 0) ->
		win_newproperties(Win, [floating|Rest]) % dialogs and transient wins should be floating on top
	; true),
	(win_mon_ws(ParentWin, TMon, TWs) -> % transient wins should be opened where the parent win is
		win_tomon_toworkspace_top(Win, TMon, TWs, true)
	; true),

	(StateProperty == NetWMStateFullscreen -> win_fullscreen(Win, true) ; true)
.

%update_sizehints(Win) :-
%	display(Dp),
%	plx:x_get_wm_normal_hints(Dp, Win, [Flags, _, _, _, _, MinW, MinH, MaxW, MaxH, WInc, HInc,
%	                                    MinAX, MinAY, MaxAX, MaxAY, BW, BH, WGrav], Status),
%	(Status =\= 0 ->
%		PSize      is 1 << 3,  % program specified size
%		PMinSize   is 1 << 4,  % program specified minimum size
%		PMaxSize   is 1 << 5,  % program specified maximum size
%		PResizeInc is 1 << 6,  % program specified resize increments
%		PAspect    is 1 << 7,  % program specified min and max aspect ratios
%		PBaseSize  is 1 << 8,  % program specified base for incrementing
%
%		% CONTINUE FROM HERE
%
%		true
%	; true)
%.

%! ruletest_on(++Test:term, ++Str:term) is det
%
%  Tests whether the given string passes the specified Test according these rules:
%  - if Str is a variable, it always passes regardless of Test
%  - if Str is a string, it passes if it's a substring of Test
%  - if Str is a exact(S) where S is a string, then S must be equal to Test
%
%  @arg Test to check Str against, must be a string or exact(S) where S is a string
%  @arg Str string or var to check if it passes Test
ruletest_on(Test, _) :- var(Test).
ruletest_on(Test, Str) :- chars_si(Test), sub_string(Str, _, _, _, Test).
ruletest_on(exact(Str), Str).

%! apply_rules(++Win:integer) is det
%
%  Checks if the specified window matches any of rules/1, in case of a match,
%  applies mode of said rule (only that of the first match).
%  Checks include: name, class and title of the window, see XGetClassHint(3)
%  Modes include: managed, floating, [X,Y,W,H], fullscreen
%
%  For more details, see description of rules/1.
%
%  @arg Win XID of window to check and apply rules for
apply_rules(Win) :-
	rules(Rules),
	(Rules \= [], display(Dp), XA_WM_NAME is 39,
	plx:x_get_class_hint(Dp, Win, WName, WClass),
	plx:x_get_text_property(Dp, Win, WTitle, XA_WM_NAME, Status), Status =\= 0 ->
		once((member((Name, Class, Title -> Mon, Ws, Mode), Rules),
		ruletest_on(Name,  WName),
		ruletest_on(Class, WClass),
		ruletest_on(Title, WTitle) ->
			active_mon_ws(ActMon, _),
			(ground(Mon), monitors(Mons), member(Mon, Mons) -> ToMon = Mon ; ToMon = ActMon),

			global_key_value(active_ws, ToMon, ActWsOnToMon),
			(ground(Ws), nb_getval(workspaces, Wss),
			(atom(Ws) -> member(Ws, Wss), ToWs = Ws ; nth1(Ws, Wss, ToWs, _)) -> true ; ToWs = ActWsOnToMon),

			global_key_value(free_win_space, ActMon, [MX, MY, MW, MH]),
			win_properties(Win, [_, Fullscr, [OldX, OldY, OldW, OldH]]),
			(nonvar(Mode) ->
				(Mode = floating ->
					win_newproperties(Win, [floating, Fullscr, [OldX, OldY, OldW, OldH]])
				; Mode = [X, Y, W, H] ->
					(var(W) -> NewW is OldW
					;EW is W, float(EW) -> NewW is round(MW * EW) ; NewW is W),
					(var(H) -> NewH is OldH
					;EH is H, float(EH) -> NewH is round(MH * EH) ; NewH is H),
					(var(X)     -> NewX is OldX
					;X = left   -> NewX is MX
					;X = right  -> NewX is MX + MW - NewW
					;X = center -> NewX is round(MX + MW / 2 - NewW / 2)
					;EX is X, (float(EX) -> NewX is MX + round(MW * EX) ; NewX is MX + X)),
					(var(Y)     -> NewY is OldY
					;Y = top    -> NewY is MY
					;Y = bottom -> NewY is MY + MH - NewH
					;Y = center -> NewY is round(MY + MH / 2 - NewH / 2)
					;EY is Y, (float(EY) -> NewY is MY + round(MH * EY) ; NewY is MY + Y)),

					plx:x_move_resize_window(Dp, Win, NewX, NewY, NewW, NewH),
					win_newproperties(Win, [floating, Fullscr, [NewX, NewY, NewW, NewH]])
				; Mode = fullscreen ->
					win_fullscreen(Win, true)
				; true)
			; true),
			win_tomon_toworkspace_top(Win, ToMon, ToWs, true)
		; true))
	; true)
.

%! cmp_mons(+[X, Y, W, H]:[integer], +Mon1:string, +Mon2:string) is semidet
%
%  Between two monitors, checks which of the two contains the specified rectangle
%  "more", i.e. which of the two contains the larger area from the rectangle.
%  Succeeds if Mon1 <= Mon2 in the above regard.
%
%  @arg [X, Y, W, H] rectangle to check containment of
%  @arg Mon1 first monitor to compare containment
%  @arg Mon2 second monitor to compare containment
cmp_mons([X, Y, W, H], Mon1, Mon2) :-
	global_key_value(monitor_geom, Mon1, [M1X, M1Y, M1W, M1H]),
	global_key_value(monitor_geom, Mon2, [M2X, M2Y, M2W, M2H]),
	Area1 is max(0, min(X + W, M1X + M1W) - max(X, M1X)) * max(0, min(Y + H, M1Y + M1H) - max(Y, M1Y)),
	Area2 is max(0, min(X + W, M2X + M2W) - max(X, M2X)) * max(0, min(Y + H, M2Y + M2H) - max(Y, M2Y)),
	Area1 =< Area2
.

%! rect_inmon(++Rect:[integer], -InMon:string) is det
%
%  Determines which monitor contains a rectangle geometry (window) the "most",
%  i.e. the one that contains the most area from the given geometry.
%
%  @arg Rect [X,Y,W,H] geometry to check containment of
%  @arg InMon name of monitor which contains the most area from Rect
rect_inmon(Rect, InMon) :- monitors(Mons), max_member(cmp_mons(Rect), InMon, Mons).

%! unmanage(++Win:integer) is det
%
%  If the specified window was managed, unregisters it from plwm.
%  It will be removed from the internal state; relevant atoms will be updated;
%  re-focus and re-layout will be performed if needed.
%
%  Should be called on DestroyNotify and UnmapNotify events.
%
%  @arg Win XID of window to unmanage
unmanage(Win) :-
	(win_mon_ws(Win, Mon, Ws) ->
		global_key_value(windows, Mon-Ws, Wins),
		(once(nth0(Idx, Wins, Win, RemainingWins)) ->
			global_key_newvalue(windows, Mon-Ws, RemainingWins),
			term_to_atom(Win, WinAtom), nb_delete(WinAtom),
			update_clientlist,
			update_ws_atoms,
			active_mon_ws(ActMon, ActWs),
			(global_key_value(focused, Mon-Ws, Win) ->
				NextIdx is max(0, Idx - 1),
				(nth0(NextIdx, RemainingWins, PrevWin) ->
					global_key_newvalue(focused, Mon-Ws, PrevWin),
					(Mon-Ws == ActMon-ActWs -> % predecessor win (if any) gets the focus
						focus(PrevWin),
						raise(PrevWin)
					; true)
				;
					global_key_newvalue(focused, Mon-Ws, 0),
					display(Dp), rootwin(Rootwin), netatom(activewindow, NetActiveWindow),
					plx:x_delete_property(Dp, Rootwin, NetActiveWindow)
				)
			; true),
			(Ws == ActWs ->
				layout:relayout(Mon-Ws)
			; true)
		; true)
	; true)
.

%! handle_event(++EventType:atom, ++EventArgs:[term]) is det
%
%  Handles X11 events returned by XNextEvent(3).
%
%  @arg EventType type of the X11 event (e.g. keypress, enternotify, propertynotify)
%  @arg EventArgs arguments from the X11 event, different for each event type
handle_event(EventType, EventArgs) :- call_with_error_context(handle_event_(EventType, EventArgs), predicate-handle_event/2).

handle_event_(keypress, [_, _, _, _, _, _, _, _, _, _, _, State, Keycode, _]) :-
	(keymap_internal(Keycode, State, Action) ->
		catch(ignore(Action), Ex, (writeln(Ex), true))
	; true)
.

handle_event_(buttonpress, [_, _, Dp, _, _, Subwin, _, _, _, Xroot, Yroot, _, Button, _]) :-
	Button1 is 1, Button3 is 3, Button4 is 4, Button5 is 5,

	% scrolled up
	(Button = Button4 ->
		(scroll_up_action(Action), Action \= none ->
			catch(ignore(Action), Ex, (writeln(Ex), true))
		; true)

	% scrolled down
	;Button = Button5 ->
		(scroll_down_action(Action), Action \= none ->
			catch(ignore(Action), Ex, (writeln(Ex), true))
		; true)

	% left or right mouse clicked
	;(Button = Button1 ; Button = Button3) ->
		nb_getval(bars, Bars),
		(Subwin =\= 0, \+ member(Subwin, Bars) ->
			focus(Subwin),
			raise(Subwin),
			(plx:x_get_window_attributes(Dp, Subwin, [X, Y, W, H], Status), Status =\= 0,
			win_properties(Subwin, [_, false|_]) ->  % don't allow dragging fullscreen wins
				nb_setval(drag_initial_winattr, [X, Y, W, H]),
				nb_setval(dragged, [Subwin, Xroot, Yroot, Button])
			; true)
		; true)
	; true)
.

handle_event_(buttonrelease, _) :-
	nb_setval(dragged, none)
.

handle_event_(enternotify, [_, _, _, _, Win, _, _, _, _, _, _, _, _, _, _, _, _]) :-
	rootwin(Rootwin),
	(Win =\= Rootwin -> focus(Win) ; true)
.

handle_event_(motionnotify, [_, _, Dp, _, _, _, _, _, _, Xroot, Yroot |_]) :-
	(nb_getval(dragged, [Win, SXroot, SYroot, SButton]), Win =\= 0,
	plx:x_get_window_attributes(Dp, Win, WinGeom, Status), Status =\= 0 ->
		rect_inmon(WinGeom, Mon),
		switch_monitor(Mon),
		active_mon_ws(Mon, ActWs),
		win_tomon_toworkspace_top(Win, Mon, ActWs, false), % dragged to other monitor

		nb_getval(drag_initial_winattr, [AX, AY, AW, AH]),
		border_width(BorderW),
		Xdiff is Xroot - SXroot,
		Ydiff is Yroot - SYroot,
		(SButton is 1 -> NewX is AX + Xdiff ; NewX is AX),
		(SButton is 1 -> NewY is AY + Ydiff ; NewY is AY),
		(SButton is 3 -> NewW is max(1, AW + Xdiff) ; NewW is max(1, AW)),
		(SButton is 3 -> NewH is max(1, AH + Ydiff) ; NewH is max(1, AH)),

		% Apply snapping to screen edge
		snap_threshold(SnapPixel),
		(SnapPixel = 0 ->
			plx:x_move_resize_window(Dp, Win, NewX, NewY, NewW, NewH),
			FinalX is NewX, FinalY is NewY, FinalW is NewW, FinalH is NewH
		;
			global_key_value(free_win_space, Mon, [MX, MY, MW, MH]),
			BdiffL is abs(MX - NewX), BdiffR is abs((MX + MW) - (NewX + NewW)),
			BdiffT is abs(MY - NewY), BdiffB is abs((MY + MH) - (NewY + NewH)),
			(SButton is 1 -> % window moved
				(BdiffL =< SnapPixel -> NewXSnap is MX
				;BdiffR =< SnapPixel -> NewXSnap is MX + MW - NewW - 2 * BorderW
				;NewXSnap is NewX),
				(BdiffT =< SnapPixel -> NewYSnap is MY
				;BdiffB =< SnapPixel -> NewYSnap is MY + MH - NewH - 2 * BorderW
				;NewYSnap is NewY),
				NewWSnap is NewW,
				NewHSnap is NewH
			;SButton is 3 -> % window resized
				% Note: resizing is always directed to the right and/or bottom
				(BdiffR =< SnapPixel -> NewWSnap is NewW + BdiffR - 2 * BorderW
				;NewWSnap is NewW),
				(BdiffB =< SnapPixel -> NewHSnap is NewH + BdiffB - 2 * BorderW
				;NewHSnap is NewH),
				NewXSnap is NewX,
				NewYSnap is NewY
			; true),
			plx:x_move_resize_window(Dp, Win, NewXSnap, NewYSnap, NewWSnap, NewHSnap),
			FinalX is NewXSnap, FinalY is NewYSnap, FinalW is NewWSnap, FinalH is NewHSnap),

		global_value(layout, Layout),  % become unmanaged when moved/resized in non-floating layout
		win_properties(Win, [PrevState, Fullscr|_]),
		(Layout = floating, PrevState = managed -> NewState = managed ; NewState = floating),
		win_newproperties(Win, [NewState, Fullscr, [FinalX, FinalY, FinalW, FinalH]]),

		(Layout \= floating, PrevState = managed -> layout:relayout ; true)
	;
		rect_inmon([Xroot, Yroot, 1, 1], Mon),
		switch_monitor(Mon) % pointer might got moved to other monitor
	)
.

handle_event_(keyrelease, _).

handle_event_(maprequest, [_, _, _, Dp, _, Win]) :-
	active_mon_ws(ActMon, ActWs),
	(plx:x_get_window_attributes(Dp, Win, Geom, Status), Status =\= 0 ->
	(plx:x_get_class_hint(Dp, Win, Name, Class),
	bar_classes(BarClasses), member(Name-Class, BarClasses) ->
		nb_getval(bars, Bars),
		(\+ memberchk(Win, Bars) ->
			nb_setval(bars, [Win|Bars]),
			plx:x_map_window(Dp, Win),
			(bar_placement(follow_focus) ->
				rect_inmon(Geom, InMon),
				shiftcoord_win_from_to(Win, InMon, ActMon)
			; true),
			update_free_win_space,
			WinSpawned = true
		; true)
	;
		global_value(windows, Wins),
		(\+ memberchk(Win, Wins) ->
			run_hook(window_create_pre),

			(attach_bottom(true) -> append(Wins, [Win], NewWins) ; NewWins = [Win|Wins]),
			global_newvalue(windows, NewWins),
			plx:x_map_window(Dp, Win),

			CWBorderWidth is 1 << 4,
			border_width(BorderW),
			nb_getval(border_pixel, BorderPixel),
			plx:x_configure_window(Dp, Win, CWBorderWidth, 0, 0, 0, 0, BorderW, 0, 0),
			plx:x_set_window_border(Dp, Win, BorderPixel),

			EnterWindowMask is 1 << 4,  StructureNotifyMask is 1 << 17,
			FocusChangeMask is 1 << 21, PropertyChangeMask  is 1 << 22,
			EventMask is EnterWindowMask \/ StructureNotifyMask \/ FocusChangeMask \/ PropertyChangeMask,
			plx:x_select_input(Dp, Win, EventMask),

			focus(Win),
			raise(Win),
			win_newproperties(Win, [managed, false, Geom]),
			update_wintype(Win),
			update_ws_atoms,
			WinSpawned = true,

			run_hook(window_create_post)
		; true)
	); true),
	(WinSpawned = true ->
		rootwin(Rootwin), netatom(clientlist, NetClientList),
		PropModeAppend is 2, XA_WINDOW is 33,
		plx:x_change_property(Dp, Rootwin, NetClientList, XA_WINDOW, 32, PropModeAppend, [Win], 1),

		% Floating windows would spawn to (0, 0) by default,
		% adjust it to top-left of free win space, so top bars are not covered
		(global_key_value(layout, ActMon-ActWs, floating) ->
			global_key_value(free_win_space, ActMon, [BaseX, BaseY, _, _]),
			win_properties(Win,    [State, Fullscr, [    _,     _, W, H]]),
			win_newproperties(Win, [State, Fullscr, [BaseX, BaseY, W, H]]),
			plx:x_move_resize_window(Dp, Win, BaseX, BaseY, W, H)
		; true),

		apply_rules(Win), % note: this considers free win space
		layout:relayout
	; true)
.

handle_event_(unmapnotify, [_, _, SendEvent, Dp, _, Win, _]) :-
	% Set withdrawn state
	wmatom(wmstate, WMState), PropModeReplace is 0, WithdrawnState is 0, None is 0,
	plx:x_change_property(Dp, Win, WMState, WMState, 32, PropModeReplace, [WithdrawnState, None], 2),

	% Only unmanage the window if unmap was initiated by a user action
	(SendEvent = false ->
		unmanage(Win)
	; true)
.

handle_event_(destroynotify, [_, _, _, _, _, Win]) :-
	nb_getval(bars, Bars),
	(selectchk(Win, Bars, RemainingBars) ->  % bar is removed, get back its space
		nb_setval(bars, RemainingBars),
		update_free_win_space
	;
		run_hook(window_destroy_pre),
		unmanage(Win),
		run_hook(window_destroy_post)
	)
.

handle_event_(propertynotify, [_, _, _, Win, Atom, _, _]) :-
	XA_WM_TRANSIENT_FOR is 86,
	netatom(wmwindowtype, NetWMWindowType), netatom(wmstate, NetWMState),

	(Atom == XA_WM_TRANSIENT_FOR, win_properties(Win, [_|Rest]) ->
		win_newproperties(Win, [floating|Rest])
	; true),

	(Atom == NetWMWindowType ; Atom == NetWMState ->
		update_wintype(Win)
	; true)
.

handle_event_(clientmessage, [_, _, _, _, Win, MessageType, _, DataL0, DataL1, DataL2]) :-
	netatom(currentdesktop, NetCurrentDesktop),
	netatom(wmstate, NetWMState),
	netatom(wmstatefullscreen, NetWMStateFullscreen),
	(MessageType == NetWMState, (DataL1 == NetWMStateFullscreen ; DataL2 == NetWMStateFullscreen) ->
		(DataL0 == 0 -> win_fullscreen(Win, false)  % _NET_WM_STATE_REMOVE
		;DataL0 == 1 -> win_fullscreen(Win, true)   % _NET_WM_STATE_ADD
		;DataL0 == 2 -> (                           % _NET_WM_STATE_TOGGLE
			win_properties(Win, [_, Fullscr, _]),
			utils:bool_negated(Fullscr, NFullscr),
			win_fullscreen(Win, NFullscr))
		; true)
	; MessageType == NetCurrentDesktop ->
		NewWsIdx is DataL0 + 1,
		switch_workspace(NewWsIdx)
	; true)
.

handle_event_(configurenotify, [_, _, _, _, _, Win, _, _, _, _, _, _, _]) :-
	rootwin(Rootwin),
	(Win == Rootwin ->
		% Notice of pending jobs is implemented with a ConfigureNotify
		(utils:jobs(Jobs) ->
			forall(member(Job, Jobs),
				ignore(catch(call(Job), Ex, (writeln(Ex), true)))
			),
			retract(utils:jobs(_))
		; true)
	; true)
.

handle_event_(rrscreenchangenotify, _) :-
	monitors(Mons),
	query_outputs(OutputInfos),
	forall(member(Output-Geom, OutputInfos), (

		% Monitor already managed
		(member(Output, Mons) ->

			% Update geometry if it changed
			global_key_value(monitor_geom, Output, PrevGeom),
			(PrevGeom \= Geom ->
				global_key_newvalue(monitor_geom, Output, Geom),
				format("Monitor \"~s\" geometry reconfigured~n", [Output])
			; true)

		% Add new monitor
		;
			init_monitor(Output, Geom)
		)
	)),

	% Remove no longer connected monitors
	forall((member(Mon, Mons), \+ member(Mon-_, OutputInfos)), (
		delete_monitor(Mon)
	)),

	update_free_win_space
.

%! send_event(++Win:integer, ++Protocol:atom) is semidet
%
%  Sends a ClientMessage XEvent for the given window using the specified
%  communication protocol (see WM_PROTOCOLS).
%  Fails without any side-effect if the protocols cannot be queried, the window
%  does not support the desired one or if the event creation fails.
%
%  @arg Win XID of window to send event for
%  @arg Protocol communication protocol to use
send_event(Win, Protocol) :-
	display(Dp),
	plx:x_get_wm_protocols(Dp, Win, Protocols, _),

	(member(Protocol, Protocols) ->
		NoEventMask is 0, CurrentTime is 0,
		wmatom(wmprotocols, WmProtocols),

		plx:create_clientmessage_event(Win, WmProtocols, 32, Protocol, CurrentTime, ClientMessage),
		plx:x_send_event(Dp, Win, false, NoEventMask, ClientMessage),
		plx:c_free(ClientMessage)
	)
.

%! jobs_notify(++Jobs:[predicate]) is det
%
%  Asserts a list of predicates to the jobs/1 dynamic predicate which is
%  accessible in all threads.
%  Then it creates an X11 ConfigureEvent to notify the eventloop of the main thread
%  of pending jobs.
%  The jobs will be processed (and emptied from jobs/1) in handle_event(configurenotify, _).
%  This predicate can be safely called from other threads.
%
%  @arg Jobs list of predicates to execute by the main thread
jobs_notify(Jobs) :-
	display(Dp), rootwin(Rootwin),
	(plx:create_configure_event(Dp, Rootwin, ConfigureEvent) ->
		% We can pass jobs from other threads using a dynamic predicate
		assertz(jobs(Jobs)),

		StructureNotifyMask is 1 << 17,
		plx:x_send_event(Dp, Rootwin, false, StructureNotifyMask, ConfigureEvent),
		plx:x_sync(Dp, false), % flush the event queue
		plx:c_free(ConfigureEvent)
	; true)
.

%! win_mon_ws(++Win:integer, -Mon:string, -Ws:atom) is det
%
%  Returns the containing monitor and workspace of the specified window.
%
%  @arg Win XID of window to return monitor-workspace of
%  @arg Mon containing monitor of Win
%  @arg Ws containing workspace of Win
win_mon_ws(Win, Mon, Ws) :-
	nb_getval(windows, AWins), assoc_to_keys(AWins, Keys),
	once((member(Mon-Ws, Keys), global_key_value(windows, Mon-Ws, Wins), member(Win, Wins)))
.

%! win_properties(++Win:integer, -Properties:[term]) is det
%
%  Fetches the properties of the specified window in the following format:
%  [State, Fullscr, [X,Y,W,H]] where State is managed or floating,
%                                    Fullscr is bool,
%                                    X,Y,W,H are integers
%
%  @arg Win XID of window to fetch the properties for
%  @arg Properties property list of Win
win_properties(Win, Properties) :- term_to_atom(Win, WinAtom), nb_current(WinAtom, Properties).

%! win_newproperties(++Win:integer, ++Properties:[term]) is det
%
%  Sets the properties of the specified window in the following format:
%  [State, Fullscr, [X,Y,W,H]] where State is managed or floating,
%                                    Fullscr is bool,
%                                    X,Y,W,H are integers
%
%  @arg Win XID of window to set the properties for
%  @arg Properties new property list for Win
win_newproperties(Win, Properties) :- term_to_atom(Win, WinAtom), nb_setval(WinAtom, Properties).

%! eventloop() is det
%
%  Main event loop of plwm.
%  It fetches the next X11 event with XNextEvent(3) then calls handle_event/2 on it.
%  After the event is handled, eventloop/0 calls itself recursively.
%
%  Note: the recursive call is the tail of the term, so we get last call optimization.
eventloop :-
	display(Dp),

	plx:x_next_event(Dp, Event),
	(Event = [EventType|EventArgs] ->
		handle_event(EventType, EventArgs)
	; true),  % simply ignore "unsupported_event" cases

	eventloop
.

%! change_nmaster(++N:term) is det
%
%  Changes nmaster by either an offset (e.g. -1, +2) or to a specific value (e.g. 0, 3).
%
%  @arg N can be +X, -X or X where X is an integer to offset or set the nmaster
change_nmaster(N) :-
	global_value(nmaster, Nmaster),
	(N = +Delta, integer(Delta) -> NewNmaster is Nmaster + Delta     % increase
	;integer(N), N < 0          -> NewNmaster is max(0, Nmaster + N) % decrease
	;integer(N)                 -> NewNmaster is N                   % set
	; true),
	(integer(NewNmaster) ->
		global_newvalue(nmaster, NewNmaster),
		layout:relayout
	;
		utils:warn_invalid_arg("change_nmaster", N))
.

%! change_mfact(++F:term) is det
%
%  Changes mfact by either an offset (e.g. -0.05, +0.05) or to a specific value (e.g. 0.5, 0.8).
%
%  Note: F = N/D format is also accepted where N and D are integers.
%
%  @arg F can be +X, -X or X where X is a float to offset or set the mfact
change_mfact(F) :-
	global_value(mfact, Mfact),
	(F = +Delta, float(Delta) -> NewMfact is min(Mfact + Delta, 0.95) % increase
	;float(F), F < 0          -> NewMfact is max(Mfact + F,    0.05)  % decrease
	;utils:is_float(F), 0.05 =< F, F =< 0.95 -> NewMfact is F         % set
	; true),
	(float(NewMfact) ->
		global_newvalue(mfact, NewMfact),
		layout:relayout
	;
		utils:warn_invalid_arg("change_mfact", F))
.

%! focused_to_top() is det
%
%  Moves the currently focused window to the top of window stack.
focused_to_top :-
	global_value(focused, FocusedWin),
	(FocusedWin =\= 0 ->
		global_value(windows, Wins),
		selectchk(FocusedWin, Wins, RemainingWins),
		global_newvalue(windows, [FocusedWin|RemainingWins]),
		layout:relayout
	; true)
.

%! move_focused(++Direction:term) is det
%
%  Moves the currently focused window one place up or down the window stack.
%
%  @arg Direction can be up or down
move_focused(Direction) :-
	global_value(windows, Wins), global_value(focused, FocusedWin),
	length(Wins, WinCnt),
	(1 < WinCnt, FocusedWin =\= 0 ->
		(Direction = down ->
			(utils:swap_with_next(Wins, FocusedWin, SwapResult) ->
				NewWins = SwapResult
			;
				selectchk(FocusedWin, Wins, OtherWins), % wrap to first
				NewWins = [FocusedWin|OtherWins])
		; Direction = up ->
			(utils:swap_with_prev(Wins, FocusedWin, SwapResult) ->
				NewWins = SwapResult
			;
				Wins = [FstWin|OtherWins],              % wrap to last
				append(OtherWins, [FstWin], NewWins))
		;
			utils:warn_invalid_arg("move_focused", Direction),
			NewWins = Wins),
		(Wins \= NewWins ->
			global_newvalue(windows, NewWins),
			layout:relayout
		; true)
	; true)
.

%! trim_bar_space(++Mon:string, ++[BarX, BarY, BarW, BarH]:[integer]) is det
%
%  Logically trims the screen space from the specified monitor based on the
%  specified status bar geometry by setting the free_win_space global.
%
%  Note: if a bar is not at a screen edge, but floats somewhere, the location
%  heuristics will still detect which edge it is closest to and will also
%  trim the smaller space above/below/left/right of it.
%
%  @arg Mon name of monitor to trim the screen space from
%  @arg [BarX, BarY, BarW, BarH] window geometry of the status bar
trim_bar_space(Mon, [BarX, BarY, BarW, BarH]) :-
	global_key_value(free_win_space, Mon, [X, Y, W, H]),
	% horizontal bar
	(BarH < BarW ->
		RoughLocation is BarY / H,
		(RoughLocation < 0.5 ->
			% top bar
			NewX is X, NewY is Y + BarY + BarH, NewW is W, NewH is H - BarH
		;
			% bottom bar
			NewX is X, NewY is Y, NewW is W, NewH is H - BarH)
	% vertical bar
	;
		RoughLocation is BarX / W,
		(RoughLocation < 0.5 ->
			% left bar
			NewX is X + BarX + BarW, NewY is Y, NewW is W - BarW, NewH is H
		;
			% right bar
			NewX is X, NewY is Y, NewW is W - BarW, NewH is H)),
	global_key_newvalue(free_win_space, Mon, [NewX, NewY, NewW, NewH])
.

%! update_free_win_space() is det
%
%  Calculates and sets the free space for managed windows with the following formula:
%    monitor resolution - outer gaps - space reserved for status bars
update_free_win_space :-
	display(Dp), monitors(Mons),
	forall(member(Mon, Mons), (
		global_key_value(monitor_geom, Mon, MonGeom),
		global_key_newvalue(free_win_space, Mon, MonGeom),

		nb_getval(bars, Bars),
		forall(member(Bar, Bars), (
			(plx:x_get_window_attributes(Dp, Bar, BarGeom, Status), Status =\= 0 ->
				rect_inmon(BarGeom, InMon),
				(bar_placement(static) ->
					(Mon = InMon -> trim_bar_space(Mon, BarGeom) ; true)
				;
					global_key_value(monitor_geom, InMon, [InMonX, InMonY, _, _]),
					BarGeom = [BarX, BarY, BarW, BarH],
					BarRelativeX is BarX - InMonX,
					BarRelativeY is BarY - InMonY,
					trim_bar_space(Mon, [BarRelativeX, BarRelativeY, BarW, BarH])
				)
			; true)
		)),
		outer_gaps(GapPixel),
		(GapPixel \= 0 ->
			global_key_value(free_win_space, Mon, [X, Y, W, H]),
			NewX is min(X + GapPixel, X + W), NewW is max(1, W - 2 * GapPixel),
			NewY is min(Y + GapPixel, Y + H), NewH is max(1, H - 2 * GapPixel),
			global_key_newvalue(free_win_space, Mon, [NewX, NewY, NewW, NewH])
		; true),

		global_key_value(active_ws, Mon, ActWs),
		layout:relayout(Mon-ActWs)
	)),
	update_workarea
.

%! update_clientlist() is det
%
%  Updates the _NET_CLIENT_LIST netatom.
%    see: https://specifications.freedesktop.org/wm-spec/latest/
update_clientlist :-
	display(Dp), rootwin(Rootwin),
	netatom(clientlist, NetClientList),
	PropModeAppend is 2, XA_WINDOW is 33,

	plx:x_delete_property(Dp, Rootwin, NetClientList),
	forall((global_key_value(windows, _, Wins), member(Win, Wins)), (
		plx:x_change_property(Dp, Rootwin, NetClientList, XA_WINDOW, 32, PropModeAppend, [Win], 1)
	))
.

%! setup_hooks() is det
%
%  Registers actions to events parsed from hooks/1.
setup_hooks :-
	retractall(hook(_, _)),
	hooks(Hooks),
	forall(member((Event -> Action), Hooks), (
		assertz(hook(Event, Action))
	))
.

%! run_hook(++Event:atom) is det
%
%  If a hook is registered for Event, it's executed.
%  Exceptions are ignored, but logged. Failure of the hook is also ignored.
%
%  @arg Event which we want to execute the hook for (if any)
run_hook(Event) :-
	(hook(Event, Action) ->
		ignore(catch(call(Action), Ex, (writeln(Ex), true)))
	; true)
.

%! init_state() is det
%
%  Initializes the window manager base state, i.e. the global, per-monitor and
%  per-workspace states with defaults.
init_state :-
	workspaces(Wss),

	empty_assoc(EmptyAMonGeom),      nb_setval(monitor_geom,   EmptyAMonGeom),
	empty_assoc(EmptyAFreeWinSpace), nb_setval(free_win_space, EmptyAFreeWinSpace),
	empty_assoc(EmptyAActiveWs),     nb_setval(active_ws,      EmptyAActiveWs),
	empty_assoc(EmptyAPrevWs),       nb_setval(prev_ws,        EmptyAPrevWs),

	empty_assoc(EmptyANmaster), nb_setval(nmaster, EmptyANmaster),
	empty_assoc(EmptyAMfact),   nb_setval(mfact,   EmptyAMfact),
	empty_assoc(EmptyALayout),  nb_setval(layout,  EmptyALayout),
	empty_assoc(EmptyAFocused), nb_setval(focused, EmptyAFocused),
	empty_assoc(EmptyAWins),    nb_setval(windows, EmptyAWins),

	(query_outputs(OutputInfos), OutputInfos \= [] ->
		init_monitors(OutputInfos),
		OutputInfos = [FstMon-_|_]
	;
		FstMon = "default"
	),

	nb_setval(active_mon, FstMon),
	nb_setval(workspaces, Wss),
	nb_setval(bars, []),
	nb_setval(dragged, none),
	nb_setval(drag_initial_winattr, none)
.

%! query_outputs(-OutputInfos:[string-[int,int,int,int]]) is semidet
%
%  Returns the list of active outputs and their geometries using the XRandR extension.
%  Fails if the XRandR extension is not available (XRRQueryExtension() returns error).
%
%  @arg OutputInfos list of output information in format: Output-[X, Y, W, H]
query_outputs(OutputInfos) :-
	(xrandr_available ->
		display(Dp), rootwin(Rootwin),
		(plx:xrr_get_screen_resources(Dp, Rootwin, ScreenResources, Outputs) ->
			RR_Connected is 0,
			findall(Output-[X, Y, W, H], (
				nth0(Idx, Outputs, _),
					(plx:xrr_get_output_info(Dp, ScreenResources, Idx, [Output, Connection, Crtc]) ->
						Connection = RR_Connected ->
						plx:xrr_get_crtc_info(Dp, ScreenResources, Crtc, [X, Y, W, H]))
				),
				OutputInfos
			),
			plx:xrr_free_screen_resources(ScreenResources)
		; true)
	;
		writeln(user_error, "XRandR extension not available"),
		fail
	)
.

%! init_monitors(++OutputInfos:string-[int,int,int,int]) is det
%
%  Initializes the base state (globals) of monitors from output information of this form:
%  [Output1-[X1, Y1, W1, H1], Output2-[X2, Y2, W2, H2],...]
%
%  E.g. [ "eDP-1"-[0, 0, 1920, 1080], "HDMI-1"-[1920, 1080, 1920, 1080] ]
%
%  @arg OutputInfos list of output names mapped to their geometries
init_monitors([]).
init_monitors([Mon-[X, Y, W, H]|Rest]) :-
	init_monitor(Mon, [X, Y, W, H]),
	init_monitors(Rest)
.

%! init_monitor(++Mon:string, ++Geom:[integer]) is det
%
%  Initializes the base state (globals) of a monitor.
%
%  @arg Mon monitor name to initialize state for
%  @arg Geom geometry of Mon
init_monitor(Mon, Geom) :-
	% A monitor with this geometry is already managed, don't register mirror
	nb_getval(monitor_geom, AMonGeom), gen_assoc(OldMon, AMonGeom, Geom) ->
		format("Monitor \"~s\" has same geometry as \"~s\", ignoring it~n", [Mon, OldMon])
	;

	default_nmaster(Nmaster), default_mfact(Mfact), default_layout(Layout),
	starting_workspace(StartWs), workspaces(Wss),

	global_key_newvalue(monitor_geom, Mon, Geom),

	nb_getval(active_ws, AActiveWs), nb_getval(prev_ws, APrevWs), nb_getval(free_win_space, AFreeWinSpace),
	nb_getval(nmaster,   ANmaster),  nb_getval(mfact,   AMfact),  nb_getval(layout, ALayout),
	nb_getval(focused,   AFocused),  nb_getval(windows, AWins),

	put_assoc(Mon, AActiveWs,     StartWs, NewAActiveWs),
	put_assoc(Mon, APrevWs,       StartWs, NewAPrevWs),
	put_assoc(Mon, AFreeWinSpace, Geom,    NewAFreeWinSpace),

	utils:pair_values_with(PMonWss, Wss, Mon),
	utils:pair_keys_with(PNmaster, PMonWss, Nmaster),
	utils:pair_keys_with(PMfact,   PMonWss, Mfact),
	utils:pair_keys_with(PLayout,  PMonWss, Layout),
	utils:pair_keys_with(PFocused, PMonWss, 0),
	utils:pair_keys_with(PWins,    PMonWss, []),

	utils:list_to_oldassoc(PNmaster, ANmaster, NewANmaster),
	utils:list_to_oldassoc(PMfact,   AMfact,   NewAMfact),
	utils:list_to_oldassoc(PLayout,  ALayout,  NewALayout),
	utils:list_to_oldassoc(PFocused, AFocused, NewAFocused),
	utils:list_to_oldassoc(PWins,    AWins,    NewAWins),

	nb_setval(active_ws, NewAActiveWs), nb_setval(prev_ws, NewAPrevWs),
	nb_setval(free_win_space, NewAFreeWinSpace),
	nb_setval(nmaster,   NewANmaster),  nb_setval(mfact,   NewAMfact), nb_setval(layout, NewALayout),
	nb_setval(focused,   NewAFocused),  nb_setval(windows, NewAWins),

	% Apply per-monitor, per-workspace overrides
	layout_default_overrides(LDefOverrides),
	forall(member((Mon, Ws -> NmasterOR, MfactOR, LayoutOR), LDefOverrides), (
		(ground(Ws) -> ForWss = [Ws] ; ForWss = Wss),
		forall(member(ForWs, ForWss), (
			(ground(NmasterOR) -> global_key_newvalue(nmaster, Mon-ForWs, NmasterOR) ; true),
			(ground(MfactOR)   -> global_key_newvalue(mfact,   Mon-ForWs, MfactOR)   ; true),
			(ground(LayoutOR)  -> global_key_newvalue(layout,  Mon-ForWs, LayoutOR)  ; true)
		))
	)),
	format("Monitor \"~s\" managed~n", [Mon])
.

%! init_x() is det
%
%  Initializes X: connects to Xorg with XOpenDisplay(3), queries base values like
%  the display pointer or the root window.
%  Also queries the XRandR extension.
init_x :-
	plx:x_open_display("", Dp),           assertz(display(Dp)),
	plx:default_root_window(Dp, Rootwin), assertz(rootwin(Rootwin)),
	plx:default_screen(Dp, Screen),       assertz(screen(Screen)),
	plx:x_set_error_handler(false),

	(plx:xrr_query_extension(Dp, _, _) ->
		assertz(xrandr_available),
		RRScreenChangeNotifyMask is 1,
		plx:xrr_select_input(Dp, Rootwin, RRScreenChangeNotifyMask)
	; true)
.

%! load_custom_config() is semidet
%
%  Attempts to load the configuration module from a custom path provided by the user.
%  Fails if the file does not exist.
load_custom_config :-
	config_flag(UserC) ->
		exists_file(UserC) ->
			atom_chars(Atom, UserC),
			consult(Atom)
.

%! load_xdg_config(++PathSuffix) is semidet
%
%  Attempts to load the configuration module from under $XDG_CONFIG_HOME.
%  Fails if $XDG_CONFIG_HOME is unset or if the file does not exist.
%
%  @arg PathSuffix relative path from $XDG_CONFIG_HOME to the configuration file
load_xdg_config(PathSuffix) :-
	getenv("XDG_CONFIG_HOME", XdgConfHome) ->
		append(XdgConfHome, PathSuffix, XdgConf),
		exists_file(XdgConf) ->
			atom_chars(Atom, XdgConf),
			consult(Atom)
.

%! load_home_config(++PathSuffix) is semidet
%
%  Attempts to load the configuration module from under $HOME/.config.
%  Fails if $HOME is unset or if the file does not exist.
%
%  @arg PathSuffix relative path from $HOME/.config to the configuration file
load_home_config(PathSuffix) :-
	getenv("HOME", Home) ->
		append(Home, "/.config", HomeCDir), append(HomeCDir, PathSuffix, HomeConf),
		exists_file(HomeConf) ->
			atom_chars(Atom, HomeConf),
			consult(Atom)
.

%! load_etc_config(++PathSuffix) is semidet
%
%  Attempts to load the configuration module from under /etc.
%  Fails if the file does not exist.
%
%  @arg PathSuffix relative path from /etc to the configuration file
load_etc_config(PathSuffix) :-
	append("/etc", PathSuffix, EtcConf),
	exists_file(EtcConf) ->
		atom_chars(Atom, EtcConf),
		consult(Atom)
.

%! load_config() is det
%
%  Attempts loading the configuration file.
%  The following is tried in this order:
%    - path set with -c
%    - $XDG_CONFIG_HOME/plwm/config.pl
%    - $HOME/.config/plwm/config.pl
%    - /etc/plwm/config.pl
%
%  Note: even if none of the above works, the predicate simply succeeds since
%  the config is optional.
load_config :-
	PathSuffix = "/plwm/config.pl",
	(load_custom_config            -> writeln("-c user config loaded")
	; load_xdg_config(PathSuffix)  -> writeln("xdg config loaded")
	; load_home_config(PathSuffix) -> writeln("home config loaded")
	; load_etc_config(PathSuffix)  -> writeln("etc config loaded")
	; load_etc_config(PathSuffix)  -> writeln("etc config loaded")
	; error(existens_error(config), load_config/1)
	)
.

%! reload_config() is det
%
%  Reloads the user configuration with the following semantics:
%  - settings absent from the config will retrain their current values
%  - settings in the config which are invalid are defaulted with settings:default_set/2
%  - settings in the config which are valid are applied
reload_config :-
	% Retract all settings after making a snapshot of them
	empty_assoc(SettigValueMap0),
	nb_setval(settings_snapshot, SettigValueMap0),
	forall(setting:setting(Setting), (
		call(user:Setting, Value),
		global_key_newvalue(settings_snapshot, Setting, Value),
		compound_name_arguments(Config, Setting, [_]),
		retractall(Config)
	)),

	% Consult user config
	load_config,

	% Restore previous values of those settings which were absent from the user config
	forall(setting:setting(Setting), (
		(\+ call(Setting, Value) ->
			global_key_value(settings_snapshot, Setting, Value),
			compound_name_arguments(Config, Setting, [Value]),
			assertz(Config)
		; true)
	)),

	% Validate settings, assign default values to ill-formed ones loaded from user config
	setting:init_config(false),

	% init_config assumes it's run during initialization, so it does not trigger
	% post-set actions (e.g. relayout). Trigger those manually
	forall(setting:setting(Setting), (
		call(Setting, Value),
		set(Setting, Value)
	)),

	nb_delete(settings_snapshot)
.

%! dump_settings(++FilePath:string, ++OnlyChanged:bool) is det
%
%  Dumps current settings to a file.
%
%  @arg FilePath path of file to create
%  @arg OnlyChanged whether to only dump settings that differ from their default values
dump_settings(FilePath, OnlyChanged) :-
	open(FilePath, write, DumpFile),
	forall(setting:setting(Setting), (
		call(Setting, Value),
		((OnlyChanged = false ; \+ setting:default_set(Setting, Value)) ->
			compound_name_arguments(Config, Setting, [Value]),
			write_term(DumpFile, Config, [quoted(true), spacing(next_argument)]),
			writeln(DumpFile, ".")
		; true)
	)),
	close(DumpFile)
.

%! opts_spec(-OptSpecs:[[term]]) is det
%
%  Returns the command-line option specifications.
%  Used with standard predicates like opt_arguments/3 and opt_help/2.
%
%  @arg OptSpecs list of option specifications
opts_spec([
	[opt(help),   type(boolean),default(false), shortflags([h]),  longflags([help]),   help('display this help and exit')],
	[opt(version),type(boolean),default(false), shortflags([v]),  longflags([version]),help('display version info')],
	[opt(log),    type(atom),   default(stdout),shortflags([l]),  longflags([log]),    help('path to logfile, or \'stdout\'')],
	[opt(check),  type(boolean),default(false), shortflags(['C']),longflags([check]),  help('check config validity and exit')],
	[opt(config), type(atom),   default(unset), shortflags([c]),  longflags([config]), help('path to user config')]
]).

%! parse_opt(++Option:term) is det
%
%  Parses and evaluates command-line options.
%
%  @arg Option created by opt_arguments/3
parse_opt(help(true)) :-
	writeln("Usage: plwm [OPTION]..."),
	opts_spec(OptsSpec), opt_help(OptsSpec, Help), writeln(Help), quit.
parse_opt(version(true)) :- version(V), write("plwm version v"), writeln(V), quit.
parse_opt(config(Cnf)) :- (Cnf \= unset -> assertz(config_flag(Cnf)) ; true).
parse_opt(log(Log)) :-
	(Log \= stdout ->
		open(Log, write, S, [buffer(line)]), set_output(S),
		set_stream(S, alias(user_output)),
		set_stream(S, alias(user_error))
	; true).
parse_opt(check(true)) :- load_config, setting:init_config(true), quit.
parse_opt(help(false)). parse_opt(version(false)). parse_opt(check(false)).

%! main() is det
%
%  Entry point of plwm.
main :-
	on_signal(term, _, quit),

	opts_spec(OptsSpec),
	opt_arguments(OptsSpec, Opts, _),
	forall(member(Opt, Opts), parse_opt(Opt)),

	load_config,
	setting:init_config(false),

	init_x,
	init_state,
	alloc_colors,
	setup_wmatoms,
	setup_netatoms,
	grab_keys,
	grab_buttons,
	setup_root_win,
	update_free_win_space,
	update_ws_atoms,

	fifo:setup_fifo,
	setup_hooks,
	run_hook(start),

	eventloop
.
