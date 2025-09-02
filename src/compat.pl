:- module(compat, [

    % swi
    compound_name_arguments/3,
    % todo scryer

    % assocs

    % format
    format_string/3,
    writeln/1,

    % globals
    compat_nb_setval/2,
    compat_nb_getval/2,

    % iso_ext
    compat_forall/2,

    % lists
    compat_length/2,
    member/2,
    is_set/1,
    compat_is_list/1,
    last/2,

    % string
    double_quotes/1,
    sub_string/5

]).

:- use_module(library(lists), []).

% swi
compound_name_arguments(Compound, Name, Args) :- user:compound_name_arguments(Compound, Name, Args).

% format
format_string(FmtStr, Args, Str) :- user:init_format_string(FmtStr, Args, Str).

writeln(String) :- user:init_writeln(String).

% globals

compat_nb_setval(Var, Value) :- user:init_nb_setval(Var, Value).
compat_nb_getval(Var, Copy) :- user:init_nb_getval(Var, Copy).

% iso_ext
:- meta_predicate(compat_forall(0,0)).
compat_forall(Goal, Test) :- user:init_forall(Goal, Test).

% lists 

compat_length(Xs, L) :- user:init_length(Xs, L).
member(X, Xs) :- lists:member(X, Xs).

is_set(Xs) :- user:init_is_set(Xs).
compat_is_list(Xs) :- user:init_is_list(Xs).

last(Xs, X) :- user:init_last(Xs, X).

% string

double_quotes(S) :- user:init_double_quotes(S).
sub_string(String, Before, Len, After, SubString) :- 
    lists:append([Prefix, SubString, Suffix], String),
        length(SubString, Len),
        length(Prefix, Before),
        length(Suffix, After).
