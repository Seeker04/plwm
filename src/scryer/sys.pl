%% Prolog System Compatibility Module for Scryer Prolog

% This module should have the same interface as the other Prolog System Compatibility Modules

:- module(sys, []).

:- use_module(library(format)).

:- meta_predicate(ignore(0)).
ignore(Goal) :- (Goal -> true ; true).

writeln(String) :- format("~s~n", [String]).
writeln(Stream, String) :- format(Stream, "~s~n", [String]).

format_helper(string(Str), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str).
format_helper(chars(Str), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str).
format_helper(atom(Atom), Fmt, Args) :- !, phrase(format_(Fmt, Args), Str), atom_chars(Atom, Str).

% todo handle argument parsing and printing of help
opt_arguments(_Spec, Opts, _PosArgs) :- Opts = [config("./config/config.pl")].
opt_help(_,_).


is_set_helper([]).
is_set_helper([X | Xs]) :- maplist(dif(X), Xs), is_set_helper(Xs).
    
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
