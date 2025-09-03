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

goal_expansion(ignore(Goal), sys:ignore_helper(Goal)).

goal_expansion(opt_arguments(Spec, Opts, PosArgs), sys:opt_arguments_helper(Spec, Opts, PosArgs)).
goal_expansion(opt_help(Spec, Help), sys:opt_help_helper(Spec, Help)).

% TODO implement on_signal equivalent in scryer-prolog
goal_expansion(on_signal(_Sig, _Old, _New), true).

% files
goal_expansion(exists_file(Path), files:file_exists(Path)).

% format 
goal_expansion(writeln(String), sys:writeln_helper(String)).
goal_expansion(writeln(Stream, String), sys:writeln_helper(Stream, String)).

goal_expansion(compat_format(Fmt, Args), iso_ext:format(Fmt, Args)).
goal_expansion(compat_format(Stream, Fmt, Args), sys:format_helper(Stream, Fmt, Args)).

:- meta_predicate(compat_forall(0,0)).
compat_forall(_,_) :-not_used.

%iso_ext
goal_expansion(compat_forall(Goal, Test), sys:forall_helper(Module:Goal, Module:Test)) :- prolog_load_context(module, Module).
goal_expansion(nb_getval(Var, Val), iso_ext:bb_get(Var, Val)).
goal_expansion(nb_setval(Var, Val), iso_ext:bb_put(Var, Val)).

% lists
goal_expansion(string(Term), si:chars_si(Term)).

goal_expansion(is_list(Xs), si:list_si(Xs)).

goal_expansion(is_set(Xs), sys:is_set_helper(Xs)).

goal_expansion(last(List, Last), lists:append(_, [Last], List)).

goal_expansion(selectchk(Elem, List, Rest), once(select(Elem, List, Rest))).

goal_expansion(atom_string(Atom, Chars), sys:atom_string_helper(Atom, Chars)).

goal_expansion(sub_string(String, Before, Len, After, SubString), sys:sub_string_helper(String, Before, Len, After, SubString)).


% 
:- initialization((
   compat_format(string(Str), "scryer/init.pl initialized", []),
    writeln(Str)
)).