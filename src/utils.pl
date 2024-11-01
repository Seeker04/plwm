% MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

% Generic helper utilities that any module can use

:- module(utils, [global_key_value/3, global_key_newvalue/3, global_value/2, global_newvalue/2, shellcmd/1]).
%                 ^ these are used quite often, so it's worth exporting them

bool_negated(true, false).
bool_negated(false, true).

is_float(X) :- float(X), ! ; (X = N/D, integer(N), integer(D)).

dumpstack() :- % for debugging
	open('/tmp/plwm_stack.log', append, S),
	get_prolog_backtrace(128, Backtrace),
	print_prolog_backtrace(S, Backtrace),
	close(S)
.

startswith_char([Ch|_], Ch) :- !.
startswith_char([C|Cs], Ch) :- char_type(C, white), startswith_char(Cs, Ch).

ensure_ampersand(CmdStr, NewCmdStr) :-
	string_chars(CmdStr, CList),
	reverse(CList, CListRev),
	startswith_char(CListRev, '&') -> NewCmdStr = CmdStr ; string_concat(CmdStr, "&", NewCmdStr)
.

% Let's not block or kill ourselves
shellcmd(Cmd) :- catch(ignore((ensure_ampersand(Cmd, CmdBg), shell(CmdBg))), Ex, (writeln(Ex), true)).

warn_invalid_arg(Pred, Arg) :-
	format(string(Msg), "warning: invalid argument to ~s: ~p, ignored", [Pred, Arg]),
	writeln(user_error, Msg)
.

split_at(0, L, [], L) :- !.
split_at(N, [X|Xs], [X|Ys], Rest) :-
	Nminus1 is N - 1,
	split_at(Nminus1, Xs, Ys, Rest)
.

pair_keys_with([], [], _).
pair_keys_with([K-V|Rest], [K|Keys], V) :- pair_keys_with(Rest, Keys, V).

pair_values_with([], [], _).
pair_values_with([K-V|Rest], [V|Values], K) :- pair_values_with(Rest, Values, K).

list_to_oldassoc([], Assoc, Assoc). % like list_to_assoc/2, but appends to an existing assoc
list_to_oldassoc([K-V|Rest], OldAssoc, NewAssoc) :-
	put_assoc(K, OldAssoc, V, Assoc),
	list_to_oldassoc(Rest, Assoc, NewAssoc)
.

global_key_value(Global, Key, Value) :-
	nb_getval(Global, Assoc),
	get_assoc(Key, Assoc, Value)
.
global_key_newvalue(Global, Key, Value) :-
	nb_getval(Global, Assoc),
	put_assoc(Key, Assoc, Value, NewAssoc),
	nb_setval(Global, NewAssoc)
.
% If Key is omitted, use active monitor-workspace as key (which is 90% of usecases)
global_value(Global, Value) :-
	nb_getval(active_mon, ActMon), global_key_value(active_ws, ActMon, ActWs),
	global_key_value(Global, ActMon-ActWs, Value)
.
global_newvalue(Global, Value) :-
	nb_getval(active_mon, ActMon), global_key_value(active_ws, ActMon, ActWs),
	global_key_newvalue(Global, ActMon-ActWs, Value)
.

swap_with_prev([X1, X2|Xs], X2, [X2, X1|Xs]) :- !.
swap_with_prev([X|Xs], Y, [X|Rest]) :- X \= Y, swap_with_prev(Xs, Y, Rest).

swap_with_next([X1, X2|Xs], X1, [X2, X1|Xs]) :- !.
swap_with_next([X|Xs], Y, [X|Rest]) :- X \= Y, swap_with_next(Xs, Y, Rest).

n_item_clones(0, _, []) :- !.
n_item_clones(N, T, [T|Ts]) :-
	N > 0,
	Nminus1 is N - 1,
	n_item_clones(Nminus1, T, Ts)
.

n_step_list(1, _, [_]).
n_step_list(N, Step, [Fst, Snd|Xs]) :-
	N > 1,
	call(Step, Fst, Snd),
	Nminus1 is N - 1,
	n_step_list(Nminus1, Step, [Snd|Xs])
.

alternate_merge(L1, L2, MergedL) :- alternate_merge_(0, L1, L2, MergedL).
alternate_merge_(_, Xs, [], Xs).
alternate_merge_(_, [], Ys, Ys).
alternate_merge_(N, [X|Xs], [Y|Ys], MergedList) :-
	Nplus1 is N + 1,
	Nmod2 is N mod 2,
	(Nmod2 == 0 -> alternate_merge_(Nplus1, Xs, [Y|Ys], Rest), MergedList = [X|Rest]
	;              alternate_merge_(Nplus1, [X|Xs], Ys, Rest), MergedList = [Y|Rest])
.

pair_up_lists([], [], []).
pair_up_lists([X|Xs], [Y|Ys], [X-Y|Rest]) :- pair_up_lists(Xs, Ys, Rest).

str_withoutlastch(Str, NewStr) :- string_length(Str, Len), Lenm1 is Len - 1, sub_string(Str, 0, Lenm1, _, NewStr).

