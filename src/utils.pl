% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

% Generic helper utilities that any module can use

:- module(utils, [global_key_value/3, global_key_newvalue/3, global_value/2, global_newvalue/2, shellcmd/1, reassert/1]).
%                 ^ these are used quite often, so it's worth exporting them

:- use_module(library(assoc)).
:- use_module(library(lists)).

%! valid_callable(++Callable:callable) is det
%
%  Checks whether the predicate in the specified callable
%  1. exists
%  2. the number of its arguments matches its arity
%  Module prefix can be included for external predicates.
%
%  @arg Callable callable term to check the validity of
valid_callable(Module:Pred) :-
	functor(Pred, Func, Arity),
	current_predicate(Module:Func/Arity),
	!
.
valid_callable(Pred) :-
	functor(Pred, Func, Arity),
	current_predicate(Func/Arity)
.

%! bool_negated(++B:bool, -NotB:bool) is det
%
%  Negates a boolean value.
%
%  @arg B negation of NotB
%  @arg NotB negation of B
bool_negated(true, false).
bool_negated(false, true).

%! is_float(++F:term) is semidet
%
%  Extension of the standard float/1 predicate, which also accepts terms of the form
%  N/D where both N and D are integers.
%
%  @arg F term to test
is_float(X) :- float(X), ! ; (X = N/D, integer(N), integer(D)).

%! dumpstack() is det
%
%  Prints the current prolog stack to /tmp/plwm_stack.log, only used for debugging.
dumpstack :- % for debugging
	open('/tmp/plwm_stack.log', append, S),
	get_prolog_backtrace(128, Backtrace),
	print_prolog_backtrace(S, Backtrace),
	close(S)
.

%! startswith_char(+Chs:[char], +Ch:char) is semidet
%
%  Tests whether a string starts with a given character ignoring leading whitespaces.
%
%  @arg Chs list of characters to check
%  @arg Ch leading character to check against
startswith_char([Ch|_], Ch) :- !.
startswith_char([C|Cs], Ch) :- char_type(C, white), startswith_char(Cs, Ch).

%! ensure_ampersand(++CmdStr:string, -NewCmdStr:string) is det
%
%  If the command string does not end with an '&' ampersand sign, then appends
%  one at the end.
%
%  @arg CmdStr input command string
%  @arg NewCmdStr output command string that ends with an ampersand
ensure_ampersand(CmdStr, NewCmdStr) :-
	string_chars(CmdStr, CList),
	reverse(CList, CListRev),
	startswith_char(CListRev, '&') -> NewCmdStr = CmdStr ; string_concat(CmdStr, "&", NewCmdStr)
.

%! shellcmd(++Cmd:string) is det
%
%  Wrapper for the standard shell/1 predicate.
%  The executed command will always be run in the background, so plwm is not blocked.
%  If shell/1 fails or throws an exception, we ignore it so plwm is not killed
%  (in case of an exception, it will be logged).
%
%  @arg Cmd command to execute with the system's default shell
shellcmd(Cmd) :- catch(ignore((ensure_ampersand(Cmd, CmdBg), shell(CmdBg))), Ex, (writeln(Ex), true)).

%! warn_invalid_arg(:Pred:callable, ++Arg:term) is det
%
%  Formats and writes an invalid argument warning to user_error.
%
%  @arg Pred predicate that was called
%  @arg Arg invalid argument that was supplied to Pred
warn_invalid_arg(Pred, Arg) :-
compat_format(string(Msg), "warning: invalid argument to ~s: ~q, ignored", [Pred, Arg]),
	writeln(user_error, Msg)
.

%! split_at(++Index:integer, ++L:[term], -Left:[term], -Right:[term]) is semidet
%
%  Splits a list at the given index. Fails if index is invalid.
%  E.g.
%    If Index is 0, then Left will be empty and Right will contain the whole list.
%    If Index is 3, then Left will contain the first three elements, the rest goes into Right.
%
%  @arg Index position to split at
%  @arg L list to split
%  @arg Left left part of the list after splitting
%  @arg Right right part of the list after splitting
split_at(0, L, [], L) :- !.
split_at(N, [X|Xs], [X|Ys], Rest) :-
	Nminus1 is N - 1,
	split_at(Nminus1, Xs, Ys, Rest)
.

%! pair_keys_with(-Pairs:[term], +Keys:[term], +Value:term) is det
%
%  Pairs up a list of keys with a value.
%  E.g.
%    [] + v -> []
%    [a,b,c] + v -> [a-v,b-v,c-v]
%
%  @arg Pairs list of pairs formed from Keys and Value
%  @arg Keys list of keys
%  @arg Value to pair up the keys with
pair_keys_with([], [], _).
pair_keys_with([K-V|Rest], [K|Keys], V) :- pair_keys_with(Rest, Keys, V).

%! pair_values_with(-Pairs:[term], +Values:[term], +Key:term) is det
%
%  Pairs up a list of values with a key.
%  E.g.
%    [] + k -> []
%    [a,b,c] + k -> [k-a, k-b, k-c]
%
%  @arg Pairs list of pairs formed from Keys and Value
%  @arg Values list of values
%  @arg Key to pair up the values with
pair_values_with([], [], _).
pair_values_with([K-V|Rest], [V|Values], K) :- pair_values_with(Rest, Values, K).

%! list_to_oldassoc(++L:[term], ++OldAssoc:assoc, --NewAssoc:assoc) is det
%
%  Works like list_to_assoc/2 from library(assoc), but appends to an existing assoc.
%
%  @arg L list of key-value pairs to append to the association
%  @arg OldAssoc original association
%  @arg NewAssoc association appended with L
list_to_oldassoc([], Assoc, Assoc).
list_to_oldassoc([K-V|Rest], OldAssoc, NewAssoc) :-
	put_assoc(K, OldAssoc, V, Assoc),
	list_to_oldassoc(Rest, Assoc, NewAssoc)
.

%! global_key_value(++Global:term, ++Key:term, -Value:term) is semidet
%
%  Returns a value from a globally stored association, see: library(assoc).
%  Throws an exception if Global does not exist or its value is not an association.
%  Fails if Key does not exist in the global association.
%
%  @arg Global name of global variable that stores the association
%  @arg Key key in the global association to query
%  @arg Value stored at the given key in the global association
global_key_value(Global, Key, Value) :-
	nb_getval(Global, Assoc),
	get_assoc(Key, Assoc, Value)
.

%! global_key_newvalue(++Global:term, ++Key:term, ++Value:term) is det
%
%  Sets a value in a globally stored association, see: library(assoc).
%  Throws an exception if Global does not exist or its value is not an association.
%
%  @arg Global name of global variable that stores the association
%  @arg Key key in the global association to modify at
%  @arg Value to store at the given key in the global association
global_key_newvalue(Global, Key, Value) :-
	nb_getval(Global, Assoc),
	put_assoc(Key, Assoc, Value, NewAssoc),
	nb_setval(Global, NewAssoc)
.

%! global_value(++Global:term, -Value:term) is semidet
%
%  Like global_key_value/3, but Key defaults to the active monitor-workspace.
%
%  @arg Global name of global variable that stores the association
%  @arg Value stored at the active monitor-workspace in the global association
global_value(Global, Value) :-
	nb_getval(active_mon, ActMon), global_key_value(active_ws, ActMon, ActWs),
	global_key_value(Global, ActMon-ActWs, Value)
.

%! global_newvalue(++Global:term, ++Value:term) is det
%
%  Like global_key_newvalue/3, but Key defaults to the active monitor-workspace.
%
%  @arg Global name of global variable that stores the association
%  @arg Value to store at the active monitor-workspace in the global association
global_newvalue(Global, Value) :-
	nb_getval(active_mon, ActMon), global_key_value(active_ws, ActMon, ActWs),
	global_key_newvalue(Global, ActMon-ActWs, Value)
.

%! swap_with_prev(++L:[term], ++Value:term, -NewL:[term]) is semidet
%
%  Finds the first instance of a value in a list and (if any) swaps it with its predecessor.
%  If Value is not present or has no predecessor, the predicate fails.
%
%  @arg L subject list of terms
%  @arg Value to swap with the preceding one
%  @arg NewL L, but where the first instance of Value is swapped with its predecessor
swap_with_prev([X1, X2|Xs], X2, [X2, X1|Xs]) :- !.
swap_with_prev([X|Xs], Y, [X|Rest]) :- X \= Y, swap_with_prev(Xs, Y, Rest).

%! swap_with_next(++L:[term], ++Value:term, -NewL:[term]) is semidet
%
%  Finds the first instance of a value in a list and (if any) swaps it with its successor.
%  If Value is not present or has no successor, the predicate fails.
%
%  @arg L subject list of terms
%  @arg Value to swap with the subsequent one
%  @arg NewL L, but where the first instance of Value is swapped with its successor
swap_with_next([X1, X2|Xs], X1, [X2, X1|Xs]) :- !.
swap_with_next([X|Xs], Y, [X|Rest]) :- X \= Y, swap_with_next(Xs, Y, Rest).

%! n_item_clones(++N:integer, +T:term, -Ts:[term]) is semidet
%
%  Clones an arbitrary term to a list of copies.
%
%  @arg N number of clones to create
%  @arg T term to clone
%  @arg Ts list of cloned terms
n_item_clones(0, _, []) :- !.
n_item_clones(N, T, [T|Ts]) :-
	N > 0,
	Nminus1 is N - 1,
	n_item_clones(Nminus1, T, Ts)
.

%! n_step_list(++N:integer, :Step:callable, -L:[integer]) is semidet
%
%  Generates a list of integers from a partial list by executing a Step predicate multiple times.
%  The resulting list will contain N elements.
%  E.g.
%    ?- utils:n_step_list(4, plus(5), [10|L]).
%    L = [15, 20, 25]
%
%  @arg N number of values L must contain, meaning N-1 executions of Step
%  @arg Step predicate that takes and returns an integer, e.g. plus(1)
%  @arg L list generated by N-1 subsequent executions of Step on the initial value
n_step_list(1, _, [_]).
n_step_list(N, Step, [Fst, Snd|Xs]) :-
	N > 1,
	call(Step, Fst, Snd),
	Nminus1 is N - 1,
	n_step_list(Nminus1, Step, [Snd|Xs])
.

%! alternate_merge(+L1:[term], +L2:[term], -MergedL:[term]) is det
%
%  Merges two lists in an alternating manner. First element comes from L1.
%  E.g.
%    alternate_merge([A, B, C] , [X, Y, Z] , [A, X, B, Y, C, Z])
%  If any of the list runs out of elements, the other list will be appended at the end.
%
%  @arg L1 first list
%  @arg L2 second list
%  @arg MergedL merged list
alternate_merge(L1, L2, MergedL) :- alternate_merge_(0, L1, L2, MergedL).
alternate_merge_(_, Xs, [], Xs).
alternate_merge_(_, [], Ys, Ys).
alternate_merge_(N, [X|Xs], [Y|Ys], MergedList) :-
	Nplus1 is N + 1,
	Nmod2 is N mod 2,
	(Nmod2 == 0 -> alternate_merge_(Nplus1, Xs, [Y|Ys], Rest), MergedList = [X|Rest]
	;              alternate_merge_(Nplus1, [X|Xs], Ys, Rest), MergedList = [Y|Rest])
.

%! pair_up_lists(+L1:[term], +L2:[term], -Pairs:[term]) is det
%
%  Pairs up (zips) two lists into a list of pairs.
%  E.g.
%    pair_up_lists([a, b, c] , [x, y, z] , [a-x, b-y, c-z])
%
%  @arg L1 first list (keys)
%  @arg L2 second list (values)
%  @arg Pairs list of pairs formed from L1 (keys) and L2 (values)
pair_up_lists([], [], []).
pair_up_lists([X|Xs], [Y|Ys], [X-Y|Rest]) :- pair_up_lists(Xs, Ys, Rest).

%! str_withoutlastch(++Str:string, -NewStr:string) is det
%
%  Removes the last character from a string.
%  E.g.
%    str_withoutlastch("abc\n", "abc")
%
%  @arg Str input string
%  @arg NewStr output string, i.e. Str without its last character
str_withoutlastch(Str, NewStr) :- string_length(Str, Len), Lenm1 is Len - 1, sub_string(Str, 0, Lenm1, _, NewStr).


%! reassert(++Callable:callable) is det
%
%  First, retracts all rules of the given predicate (arity is fixed),
%  then asserts with the new arguments.
%
%  E.g. if foo(a, b) holds, then after reassert(foo(x, y)) only foo(x, y) will hold.
%
%  Note: this predicate cannot go inside the utils module, because it would
%  automatically assert under the utils: namespace.
%
%  @arg Callable predicate to re-assert
reassert(Pred) :-
	functor(Pred, Name, Arity),
	utils:n_item_clones(Arity, _, Placeholders),
	compound_name_arguments(PredToRetract, Name, Placeholders), % e.g. foo(_, _)
	retractall(PredToRetract),
	assertz(Pred)
.
