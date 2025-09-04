%% Prolog System Compatibility Module for Scryer Prolog

% This module should have the same interface as the other Prolog System Compatibility Modules

:- module(sys, []).

:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(iso_ext), [forall/2]).
:- use_module(library(lists)).
:- use_module(library(os)).

:- meta_predicate(ignore_helper(0)).
ignore_helper(Goal) :- (Goal -> true ; true).

% todo handle argument parsing and printing of help
opt_arguments_helper(_Spec, Opts, _PosArgs) :- Opts = [config('./config/config.pl')].
opt_help_helper(_,_).

% format
writeln_helper(String) :- format("~s~n", [String]).
writeln_helper(Stream, String) :- format(Stream, "~s~n", [String]).

format_helper(string(Str), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str).
format_helper(chars(Str), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str).
format_helper(atom(Atom), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str), atom_chars(Atom, Str).

% iso_ext

:- meta_predicate(forall_helper(0,0)).
forall_helper(Goal, Test) :-
    forall(Goal, Test).

% lists

is_set_helper([]).
is_set_helper([X | Xs]) :- maplist(dif(X), Xs), is_set_helper(Xs).

:- meta_predicate(max_member_helper(2, -, +)).

max_member_helper(Pred, X, [X]).
max_member_helper(Pred, Max, [X0, X1 | Xs]) :- 
    call(Pred, X0, X1) -> 
        max_member_helper(Pred, Max, [X1 | Xs]) 
    ;   max_member_helper(Pred, Max, [X0 | Xs]).
    
atom_string_helper(Atom, Chars) :-
    ( var(Chars) ->
        ( atom(Atom) ->  atom_chars(Atom, Chars)
        ; si:chars_si(Atom) -> Atom = Chars
        ; number_chars(Atom, Chars)
        )
    ; var(Atom) -> (
        (atom(Chars) -> Chars = Atom
        ; si:chars_si(Chars) -> atom_chars(Atom, Chars)
        ; number_chars(Chars, C), atom_chars(Atom, C)
        )
    )).


sub_string_helper(String, Before, Len, After, SubString) :-
    append([Prefix, SubString, Suffix], String),
    length(SubString, Len),
    length(Prefix, Before),
    length(Suffix, After).

getenv_helper(AtomKey, AtomValue) :- atom_chars(AtomKey, Key), os:getenv(Key, Value), atom_chars(AtomValue, Value).