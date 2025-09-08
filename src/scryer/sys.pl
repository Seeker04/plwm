%% Prolog System Compatibility Module for Scryer Prolog

% This module should have the same interface as the other Prolog System Compatibility Modules

:- module(sys, []).

:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(os)).

% plwm only uses this with Term instantiated to an integer
% todo proper implementation?
term_to_atom_helper(Term, Atom) :- number_chars(Term, Chars), atom_chars(Atom, Chars).

:- meta_predicate(ignore_helper(0)).
ignore_helper(Goal) :- (Goal -> true ; true).

% todo handle argument parsing and printing of help
opt_arguments_helper(_Spec, Opts, _PosArgs) :- Opts = [config('./config/config.pl')].
opt_help_helper(_,_).

% between
between_helper(Min, Max, Val) :- between:between(Min, Max, Val).

% charsio

char_type_helper(' ', white).
char_type_helper('\t', white).
char_type_helper(Char, Cat) :- 
        Cat = white -> true
    ;   charsio:char_type(Char, Cat).


% format
writeln_helper(String) :- format("~q~n", [String]).
writeln_helper(Stream, String) :- format(Stream, "~q~n", [String]).

format_helper(string(Str), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str).
format_helper(chars(Str), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str).
format_helper(atom(Atom), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str), atom_chars(Atom, Str).

% iso_ext

:- meta_predicate(forall_helper(0,0)).
forall_helper(Goal, Test) :-
    forall(Goal, Test).

% todo nb_getval, nb_current should probably be implemented differently
nb_getval_helper(Var, Val) :- bb_get(Var, V) , Var \= '$undefined', Val = V.
nb_current_helper(Var, Val) :- bb_get(Var, V) , Var \= '$undefined', Val = V.
nb_delete_helper(Var) :- bb_put(Var, '$undefined').

% lists

is_set_helper([]).
is_set_helper([X | Xs]) :- maplist(dif(X), Xs), is_set_helper(Xs).

max_list_helper(List, Max) :- max_member_helper(=<, Max, List).
?- max_list_helper([pi, 4], Max).
Max = 4.

max_member_helper(Max, List) :- max_member_helper(@=<, Max, List).
?- max_member_helper(Max, [pi, 4]).
Max = pi.

:- meta_predicate(max_member_helper(2, -, +)).

max_member_helper(_Pred, X, [X]).
max_member_helper(Pred, Max, [X0, X1 | Xs]) :- 
    call(Pred, X0, X1) -> 
        max_member_helper(Pred, Max, [X1 | Xs]) 
    ;   max_member_helper(Pred, Max, [X0 | Xs]).

delete_helper([], _, []).
delete_helper([X | Xs0], Elem, Xs1) :- 
        \+(Xs0 \= Elem) -> 
            delete_helper(Xs0, Elem, Xs1) 
        ;   Xs1 = [X | Xs2], delete_helper(Xs0, Elem, Xs2).
    
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

% os

getenv_helper(AtomKey, AtomValue) :- atom_chars(AtomKey, Key), os:getenv(Key, Value), atom_chars(AtomValue, Value).

shell_helper(Command) :- os:shell(Command).
shell_helper(Command, ExitCode) :- os:shell(Command, ExitCode).