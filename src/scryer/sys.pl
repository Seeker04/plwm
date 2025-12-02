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
writeln_helper(Term) :- write_term(Term, [double_quotes(true)]), nl.
writeln_helper(Stream, Term) :- write_term(Stream, Term, [double_quotes(true)]), nl(Stream).

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

string_concat_helper(A, B, C) :-
    atom_string_helper(B, BS),
    lists:append(A, BS, C).

split_string_helper(Src, Seps, Pads, Parts) :- 
    trim_prefix(Src, Pads, Trimmed0),
    trim_suffix(Trimmed0, Pads, Trimmed1), 
    split_string_helper_(Trimmed1, Seps, Pads, Parts).

split_string_helper_(Src, Seps, Pads, [First | Rest]) :- 
    ((member(Sep, Seps), member(Sep, Src)) -> 
            once(append([P, [Sep], S], Src)),
            trim_suffix(P, Pads, First),
            trim_prefix(S, Pads, Rem),
            (Rem = "" -> 
                Rest = [] 
            ;   split_string_helper_(Rem, Seps, Pads, Rest))
        ;   First = Src, Rest = []
    ).

trim_prefix([], _, []).
trim_prefix([X0|Xs0], Pads, Xs) :- 
    member(X0, Pads) -> 
        trim_prefix(Xs0, Pads, Xs) 
    ;   Xs = [X0 | Xs0]. 

trim_suffix(Xs, Pads, Res) :- trim_suffix_(Xs, Heads, Heads, Pads, Res).

trim_suffix_([], _,_ , _, []).
trim_suffix_([X0 | Xs0], Heads0, HeadsTail0, Pads, Res) :- 
    HeadsTail0 = [X0 | HeadsTail1],
    (member(X0, Pads) ->
        trim_suffix_(Xs0, Heads0, HeadsTail1, Pads, Res)
    ;   Res = Heads0, 
        trim_suffix_(Xs0, Heads1, Heads1, Pads, HeadsTail1)
    )
.

% os

getenv_helper(AtomKey, AtomValue) :- atom_chars(AtomKey, Key), os:getenv(Key, Value), atom_chars(AtomValue, Value).

shell_helper(Command) :- os:shell(Command).
shell_helper(Command, ExitCode) :- os:shell(Command, ExitCode).

% process

process_create_helper(Exe, Args, Options) :- (path(Exe2) = Exe -> true ; Exe2 = Exe), process:process_create(Exe2, Args, Options).