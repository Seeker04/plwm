% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE
%
% This is plwm's default configuration installed under /etc
%
% You can revert this file by reinstalling plwm (if it differs, a backup will be kept)
%
% Users can create their own configs under
%     $XDG_CONFIG_HOME/plwm/config.pl
% or
%     $HOME/.config/plwm/config.pl
% to overrule this file
%
% Note: there is extensive commenting in this file, as well as arbitrary examples
% for easier understanding. Feel free to remove them once you're familiar with the settings


% :- use_module(swi/compat).
:- use_module(library(format)).

%*********************************  Layout  ***********************************

default_nmaster(1).      % initial number of master windows for master-stack layouts
default_mfact(2/3).      % initial space percentage given to master area
                         % (you can use both fractional and decimal notations)
default_layout(lmaster). % available layouts: floating, monocle, stack, hstack,
                         % nrows(N), ncols(N), grid, [lrtbc]master (see layout.pl)
attach_bottom(false).    % if true, new windows will go to the bottom of the stack


%*********************************  Borders  **********************************

border_width(1).
border_width_focused(1).
border_color("gray").
border_color_focused("blue").

% If a window is dragged this close (in pixels) to the screen edge, it will snap to it
% Set to 0 or remove the line to disable
snap_threshold(32).

% Space reserved around screen edge (floating and fullscreen windows are not affected)
% Set to 0 or remove the line to disable
outer_gaps(0).

% Gaps between adjacent tiled windows
% Set to 0 or remove the line to disable
inner_gaps(0).


%*******************************  Workspaces  *********************************

% You can have any number of these (at least 1) and can pick any names for them
% using UTF-8 characters. Just make sure they are all unique and surrounded by single quotes
workspaces(['1', '2', '3', '4', '5', '6', '7', '8', '9']).
starting_workspace('1').

% Hide names of inactive and empty workspaces from bars (removing the line implies false)
hide_empty_workspaces(false).

% The following format strings aren't fully arbitrary, they must contain either:
%   exactly 1 format argument: a ~w
%   exactly 2 format arguments: a ~d later followed by a ~w
% The ~d will be substituted by the workspace's index, and the ~w by its name
ws_format("~w").
ws_format_occupied("▘~w").
% If not specified, both will default to "~w"

% A few more examples:
%ws_format_occupied("[~w]").
%ws_format("~d: ~w").
%ws_format_occupied("▘~d: ~w").
%ws_format("[~d] ~w").
%ws_format_occupied("▘[~d] ~w").

% You can override the three layout settings from above on a per-monitor and/or
% per-workspace basis
% Underscore at the monitor column means "all monitors" for that workspace
% Underscore at the workspace column means "all workspaces" on that monitor
% Leave an underscore in the nmaster, mfact, layout columns where you don't want an override
% Note: a later override will overrule values of previous ones if there is an overlap
% Note: for technical reasons, it is important to put monitors in "" and workspaces in ''
% Examples:

layout_default_overrides([
%  monitor      workspace     nmaster  mfact   layout
% ( _        ,  '2'       ->  _     ,  _    ,  grid    ),
% ( "eDP-1"  ,  _         ->  2     ,  1/2  ,  tmaster ),
% ( "HDMI-1" ,  '3'       ->  _     ,  0.90 ,  _       )
]).

%**********************************  Bars  ************************************

% Use xprop(1) or a similar tool to find out the WM_CLASS property of your bar
% You may specify only the name: "name"-_ or only the class: _-"class"
% List multiple bar_classes if you use multiple different bars
bar_classes(["polybar"-"Polybar"]).

% Possible values:
%   follow_focus: space will be reserved for bars on all monitors and bars will
%                 always be moved to focused monitor (this is the default behavior)
%   static:       space will only be reserved for bars in their respective monitors
%                 they occupy and no bar will be moved automatically. Placing bars
%                 to desired monitors is the responsibility of the user/external bar
bar_placement(follow_focus).


%*******************************  Command FIFO  *******************************

% Predicates written to this named pipe will be executed
% Easiest way to script plwm, e.g.:
% echo "switch_workspace(next)" > /tmp/plwm_fifo
fifo_enabled(false).
fifo_path("/tmp/plwm_fifo").


%**********************************  Menus  ***********************************

% Command and its arguments to use for menu operations (e.g., menu:goto_window)
% Note: there will be a final "menu label" argument, so you should end this list
% with the prompt flag of your cmd (if any), e.g., "-p" for dmenu
menucmd(["dmenu", "-i", "-l", "20", "-p"]).

% You can also use rofi with its dmenu mode:
%menucmd(["rofi", "-dmenu"]).


%********************************  Animations  ********************************

% If turned on, movement/resizing of managed windows will be interpolated
% animation_time is in seconds
% animation_granularity means the data point count in the interpolation
animation_enabled(false).
animation_time(0.2).
animation_granularity(30).

% Note: Performance can largely vary based on various factors: hardware (GPU mostly),
% whether a compositor is running or not, compositor's config, etc.
% Everyone should experiment with the two values until the animation is tailored
% to their system and liking


%*******************************  Key bindings  *******************************

% Key you must hold for mouse operations, i.e., moving or resizing windows
% People mostly use super or alt here, but can be any modifier key (see a little below)
modkey(super).

% Action to perform on modkey + mouse scroll, for example:
% switch_workspace(next), switch_workspace(next_nonempty), switch_monitor(prev),...
% Write 'none' for no action
scroll_up_action(switch_workspace(next)).
scroll_down_action(switch_workspace(prev)).

% Format: [Mod1 + ... + Modn +] Key -> Action
%     Modifiers: shift, lock, ctrl, alt, mod2, mod3, super, mod5
%     Key must be a valid X11 key name (check xf86names.pl for special keys),
%     and if it starts with an uppercase letter (e.g., "Return", "Tab", "AudioMute"),
%     put it in quotes! (can be either single or double quotes)
%     Action: a predicate or a list of them in parentheses, e.g., (Pred1, Pred2, Pred3)
%
keymaps([
  (super +         j         ->  shift_focus(down))               ,
  (super +         k         ->  shift_focus(up))                 ,
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

  % Omit the + and - prefixes to make an exact assignment instead of a delta change

  % Layouts
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

  % You can use nrows(N) and ncols(N) for layouts with fixed number of rows/columns, e.g.:
  %super + shift + "T"       -> layout:set_layout(ncols(3))      ,

  % Workspaces
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

  % For both switch_workspace and move_focused_to_workspace, you can pass:
  %   workspace names (use single quotes)
  %   workspace indices (use numbers WITHOUT single quotes, indexed from 1)
  %   prev / next / prev_nonempty / next_nonempty (these wrap)

  % Monitors
  (super +         comma     ->  switch_monitor(prev))            ,
  (super +         period    ->  switch_monitor(next))            ,
  (super + shift + comma     ->  move_focused_to_monitor(prev))   ,
  (super + shift + period    ->  move_focused_to_monitor(next))   ,

  % For both switch_monitor and move_focused_to_monitor, you can pass:
  %   prev / next / prev_nonempty / next_nonempty (these wrap)
  %   left / right / up / down (direction relative to the active monitor, no wrapping)
  %   output names, as seen in output of xrandr(1), e.g. "eDP-1", "HDMI-1"
  %   monitor indices starting from 1

  % Menus
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

  %%%%% Custom mapping examples %%%%%

  % Launch applications
  (ctrl + shift + space      ->  shellcmd("alacritty"))           ,
  (alt + a                   ->  shellcmd("dmenu_run -l 20 -p run"))

  % Toggle status bar
 %alt + b                   ->  shellcmd("pkill polybar || polybar top") ,
 %alt + b                   ->  shellcmd("pkill polybar || (polybar top & polybar bot)") ,
 %alt + b                   ->  shellcmd("pkill -fx 'polybar top' || polybar top") ,
 %alt + shift + b           ->  shellcmd("pkill -fx 'polybar bot' || polybar bot") ,

  % Special keys (see xf86names.pl for all such keys)
 %"AudioRaiseVolume"        ->  shellcmd("pulseaudio-ctl up")   ,
 %"AudioLowerVolume"        ->  shellcmd("pulseaudio-ctl down") ,
 %"AudioMute"               ->  shellcmd("pulseaudio-ctl mute") ,
 %"MonBrightnessUp"         ->  shellcmd("xbacklight -inc 1")   ,
 %"MonBrightnessDown"       ->  shellcmd("xbacklight -dec 1")
]).


%**********************************  Rules  ***********************************

% Windows matching all the name-class-title criteria can be auto assigned to
% a predefined monitor (use double quoted output names shown by xrandr(1)) and
% workspace (use the quoted name or an index from 1) with a desired mode
% Checking is done by substring matching, wrap the strings in exact() wherever
% you wish to force an exact match
% Underscores can be used as joker values (match any) at any of the three criteria columns
% Underscore at the monitor column implies assignment to the active monitor
% Underscore at the workspace column implies assignment to the active workspace
% The mode column can be:
%   managed      : window will be managed (underscore also implies this)
%   floating     : window won't be managed (i.e. floating)
%   [X, Y, W, H] : same as floating, but also move and size to this geometry:
%                    X can be left, right, center, an integer coordinate, a percent (0.0..1.0) of x axis
%                    Y can be top, bottom, center, an integer coordinate, a percent (0.0..1.0) of y axis
%                    W can be an integer size or a percent (0.0..1.0) of screen width
%                    H can be an integer size or a percent (0.0..1.0) of screen height
%                  also, any of them can be left as _ for keeping the value the window spawned with
%   fullscreen   : window will open in fullscreen
% Note: if a window matches multiple rules, the first will be applied

% Examples:

rules([
%  name      class     title                 monitor     wspace    mode
% ( _      ,  _      ,  exact("gcolor2")  ->  _        ,  _      ,  [center, center, 1/3, 1/3] ),
% ( _      ,  _      ,  "Firefox"         ->  "eDP-1"  ,  'www'  ,  fullscreen                 ),
% ( "Bar"  ,  "Baz"  ,  _                 ->  "HDMI-1" ,  '1'    ,  [700, 250, _, _]           )
]).

% You can find out the name, class and title values of windows using xprop(1):
% WM_CLASS(STRING) = name , class
% WM_NAME(STRING)  = title


%**********************************  Hooks  ***********************************

% Run custom logic on certain events
% Syntax:
%   Event -> Action
% Event can be:
%   start                  after all initialization, but before main X event loop
%   quit                   before quitting, also before calling XCloseDisplay(3)
%   switch_workspace_pre   before switching workspace
%   switch_workspace_post  after switching workspace
%   switch_monitor_pre     before switching monitor
%   switch_monitor_post    after switching monitor
%   window_create_pre      before a window is mapped (won't run for bars)
%   window_create_post     after a window is mapped (won't run for bars)
%   window_destroy_pre     before a window is unmapped (won't run for bars)
%   window_destroy_post    after a window is unmapped (won't run for bars)
%
% Tip: you can run multiple predicates as Action by writing a comma separated list in parentheses
hooks([
  (start -> (
    format("plwm starting~n", [])
  )),

  (quit -> (
    format("plwm quitting~n", [])
  ))
]).
