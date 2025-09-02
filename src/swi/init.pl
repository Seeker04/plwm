:- set_prolog_flag(autoload, false).

% enabling this causes compilation to fail with the following cryptic error
% ERROR: '$open_wic'/2: Type error: `option' expected, found `init_file(none)' (a compound)
% :- set_prolog_flag(iso, true).

:- use_module(library(lists), []).

% assocs

% error
init_error(Exception, Src) :- throw(error(Exception, Src)).

% format
init_format_string(Fmt, Args, Str)  :- string_chars(FmtStr, Fmt), format(string(Str), FmtStr, Args).
init_writeln(String) :- format("~s~n", [String]).

% globals
init_nb_setval(Var, Value) :- nb_setval(Var, Value).
init_nb_getval(Var, Value) :- nb_getval(Var, Value).

% iso_ext

:- meta_predicate(init_forall(0,0)).
init_forall(Goal, Test) :- forall(Goal, Test).

% lists

init_length(Xs, L) :- length(Xs, L).
init_is_list(Xs) :- is_list(Xs).
init_is_set(Xs) :- lists:is_set(Xs).
init_last(List, Last) :- lists:last(List, Last).

% string

init_double_quotes(S) :- string(S).

is_char(C) :- atom_length(C, 1).