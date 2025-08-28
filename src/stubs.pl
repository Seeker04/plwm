:- module(stubs, [
    on_signal/3,
    opt_arguments/3,
    opt_help/2,
    writeln/1,
    exists_file/1,
    compound_name_arguments/3,
    nb_setval/2,
    nb_getval/2,
    atom_string/2,
    ignore/1,
    string/1,
    is_list/1,
    last/2,
    sub_string/5,
    selectchk/3
]).

:- use_module(library(format)).
:- use_module(library(files)).
:- use_module(library(error)).
:- use_module(library(si)).
:- use_module(library(lists)).
:- use_module(library(iso_ext)).

:- meta_predicate(ignore(0)).

ignore(Goal) :- (Goal -> true ; true).

on_signal(_,_,_).

is_set([]).
is_set([X | Xs]) :- \+ member(X, Xs) , is_set(Xs).

is_list(Xs) :- list_si(Xs).

last(List, Last) :- append(_, [Last], List).

sub_string(String, Before, Len, After, SubString) :-
    append([Prefix, SubString, Suffix], String),
    length(SubString, Len),
    length(Prefix, Before),
    length(Suffix, After).

selectchk(Elem, List, Rest) :- once(select(Elem, List, Rest)).

opt_arguments(_,[config("./config/config.pl")],_).

opt_help(_,_).

writeln(S) :- format("~s~n", [S]).

exists_file(Path) :- file_exists(Path).

string(S) :- chars_si(S).

compound_name_arguments(Compound, Name, Args) :- Compound =.. [Name | Args].

nb_setval(Var, Value) :-
    bb_put(Var, Value).

nb_getval(Var, Copy) :-
    bb_get(Var, Copy).

atom_string(Atom, Chars) :-
    ( var(Chars) ->
        ( atom(Atom) ->  atom_chars(Atom, Chars)
        ; chars_si(Atom) -> Atom = Chars
        ; number_chars(Atom, Chars)
        )
    ; var(Atom) -> (
        (atom(Chars) -> Chars = Atom
        ; chars_si(Chars) -> atom_chars(Atom, Chars)
        ; number_chars(Chars, C), atom_chars(Atom, C)
        )
    )).
