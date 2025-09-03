:- use_module(sys).
:- use_module(plx).

:- use_module(library(assoc), []).
:- use_module(library(format), []).
:- use_module(library(iso_ext), []).
:- use_module(library(lists), []).
:- use_module(library(pio), []).
:- use_module(library(si), []).

% swi specific
goal_expansion(use_foreign_library(_), true).

goal_expansion(compound_name_arguments(Compound, Name, Args), (Compound =.. [Name | Args])).

goal_expansion(ignore(Goal), sys:ignore(Goal)).

goal_expansion(opt_arguments(Spec, Opts, PosArgs), sys:opt_arguments(Spec, Opts, PosArgs)).
goal_expansion(opt_help(Spec, Help), sys:opt_help(Spec, Help)).

% TODO implement on_signal equivalent in scryer-prolog
goal_expansion(on_signal(_Sig, _Old, _New), true).

% files
goal_expansion(exists_file(Path), files:file_exists(Path)).

% format 
goal_expansion(writeln(String), sys:writeln(String)).
goal_expansion(format_string(Fmt, Args, Str), sys:format_string(Fmt, Args, Str)).


%iso_ext
goal_expansion(forall(Goal, Test), iso_ext:forall(Goal, Test)).
goal_expansion(nb_getval(Var, Val), iso_ext:bb_get(Var, Val)).

% lists
goal_expansion(string(Term), si:chars_si(Term)).

goal_expansion(is_list(Xs), si:list_si(Xs)).

goal_expansion(is_set(Xs), sys:is_set(Xs)).

goal_expansion(last(List, Last), lists:append(_, [Last], List)).

goal_expansion(selectchk(Elem, List, Rest), once(select(Elem, List, Rest))).

goal_expansion(atom_string(Atom, Chars), sys:atom_string(Atom, Chars)).
