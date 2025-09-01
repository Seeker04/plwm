:- module(compat, [
    on_signal/3,
    ignore/1,
    forall/2,
    load_plx/0,

    % format
    writeln/1,
    
    % files
    exists_file/1,

    % globals
    nb_setval/2,
    nb_getval/2,

    % lists 
    is_list/1,
    append/2,
    append/3,
    select/3,
    selectchk/3,
    last/2,
    is_set/1,
    length/2,
    member/2,
    nth0/3,
    nth1/3,
    maplist/3,
    foldl/4,

    % assoc
    empty_assoc/1,
    gen_assoc/3,
    put_assoc/4,
    assoc_to_keys/2,

    % string 
    atom_string/2,
    string/1,
    sub_string/5,
    format_string/3,
    
    % swi specific
    compound_name_arguments/3,
    opt_arguments/3,
    opt_help/2,
    use_foreign_library/1
]).

load_plx.

on_signal(_,_,_).

:- meta_predicate(ignore(0)).
ignore(Goal) :- (Goal -> true ; true).

:- meta_predicate(forall(0, 0)).
forall(Goal, Test) :- iso_ext:forall(Goal, Test).

use_foreign_library(_).

member(X, Xs) :- lists:member(X, Xs).
append(Xxs, Xs) :- lists:append(Xxs, Xs).
append(Xs0, Xs1, Xs2) :- lists:append(Xs0, Xs1, Xs2).

is_set([]).
is_set([X | Xs]) :- \+ member(X, Xs) , is_set(Xs).

empty_assoc(E) :- assoc:empty_assoc(E).

gen_assoc(K, Assoc, V) :- assoc:gen_assoc(K, Assoc, V).
put_assoc(K, OldAssoc, V, NewAssoc) :- assoc:put_assoc(K, OldAssoc, V, NewAssoc).
assoc_to_keys(Assoc, Keys) :- assoc:assoc_to_keys(Assoc, Keys).

is_list(Xs) :- si:list_si(Xs).

:- meta_predicate(maplist(2, ?, ?)).

maplist(Pred, Xs0, Xs1) :- lists:maplist(Pred, Xs0, Xs1).

:- meta_predicate(foldl(3, ?, ?, ?)).

foldl(Pred, Ls, A0, A) :- lists:foldl(Pred, Ls, A0, A).

nth0(Idx, Ls, E) :- lists:nth0(Idx, Ls, E).
nth1(Idx, Ls, E) :- lists:nth1(Idx, Ls, E).

last(List, Last) :- lists:append(_, [Last], List).
selectchk(Elem, List, Rest) :- once(select(Elem, List, Rest)).

length(Xs, L) :- lists:length(Xs, L).

select(X, Xs0, Xs1) :- lists:select(X, Xs0, Xs1).


sub_string(String, Before, Len, After, SubString) :-
    append([Prefix, SubString, Suffix], String),
    length(SubString, Len),
    length(Prefix, Before),
    length(Suffix, After).


opt_arguments(_,[config("./config/config.pl")],_).

opt_help(_,_).

writeln(S) :- format:format("~s~n", [S]).

exists_file(Path) :- files:file_exists(Path).

string(S) :- si:chars_si(S).

format_string(FmtStr, Args, Str) :- phrase(format:format_(FmtStr, Args), Str).

compound_name_arguments(Compound, Name, Args) :- Compound =.. [Name | Args].

nb_setval(Var, Value) :-
    iso_ext:bb_put(Var, Value).

nb_getval(Var, Copy) :-
    iso_ext:bb_get(Var, Copy).

atom_string(Atom, Chars) :-
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
