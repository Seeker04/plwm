:- use_module(library(dcgs), [phrase/2]).
:- use_module(library(dif), [dif/2]).
:- use_module(library(format), [format/2, format_//2]).
:- use_module(library(iso_ext), [forall/2]).

:- use_module(plx).


% swi
% definitions for compat with swi-prolog
% but either noops in scryer or reasonably implemented
use_foreign_library(_).

compound_name_arguments(Compound, Name, Args) :- Compound =.. [Name | Args].

:- meta_predicate(ignore(0)).
ignore(Goal) :- (Goal -> true ; true).


% todo scryer
% definitions for compat with swi-prolog that need a proper implementation
% and potentially implementation support

:- meta_predicate(on_signal(+,-,:)).

% how does swi evaluate the signal handler predicate while 
% blocked in a c-ffi call?
on_signal(_Sig,_Old,_New).

% todo handle argument parsing and printing of help
opt_arguments(_Spec, Opts, _PosArgs) :- Opts = [config("./config/config.pl")].
opt_help(_,_).

% assocs

empty_assoc(E) :- assoc:empty_assoc(E).

gen_assoc(K, Assoc, V) :- assoc:gen_assoc(K, Assoc, V).
put_assoc(K, OldAssoc, V, NewAssoc) :- assoc:put_assoc(K, OldAssoc, V, NewAssoc).
assoc_to_keys(Assoc, Keys) :- assoc:assoc_to_keys(Assoc, Keys).

% format
init_format_string(FmtStr, Args, Str) :- phrase(format_(FmtStr, Args), Str).

init_writeln(String) :- format:format("~s~n", [String]).

% files
exists_file(Path) :- files:file_exists(Path).

% global
init_nb_setval(Var, Value) :-
    iso_ext:bb_put(Var, Value).

init_nb_getval(Var, Copy) :-
    iso_ext:bb_get(Var, Copy).

% iso_ext
:- meta_predicate(init_forall(0,0)).
init_forall(Goal, Test) :- forall(Goal, Test).

% lists
init_length(Xs, L) :- lists:length(Xs, L).

init_is_list(Xs) :- si:list_si(Xs).

init_is_set([]).
init_is_set([X | Xs]) :- lists:maplist(dif:dif(X), Xs), user:init_is_set(Xs).

:- meta_predicate(maplist(2, ?, ?)).

maplist(Pred, Xs0, Xs1) :- lists:maplist(Pred, Xs0, Xs1).

:- meta_predicate(foldl(3, ?, ?, ?)).

foldl(Pred, Ls, A0, A) :- lists:foldl(Pred, Ls, A0, A).

nth0(Idx, Ls, E) :- lists:nth0(Idx, Ls, E).
nth1(Idx, Ls, E) :- lists:nth1(Idx, Ls, E).

init_last(List, Last) :- lists:append(_, [Last], List).
selectchk(Elem, List, Rest) :- once(select(Elem, List, Rest)).

length(Xs, L) :- lists:length(Xs, L).

select(X, Xs0, Xs1) :- lists:select(X, Xs0, Xs1).


append(Xxs, Xs) :- lists:append(Xxs, Xs).
append(Xs0, Xs1, Xs2) :- lists:append(Xs0, Xs1, Xs2).


% string

init_double_quotes(S) :- si:chars_si(S).

    
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

