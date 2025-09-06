:- use_module(sys).
:- use_module(plx).

:- use_module(library(assoc), []).
:- use_module(library(between), []).
:- use_module(library(error), []).
:- use_module(library(format), []).
:- use_module(library(iso_ext), []).
:- use_module(library(lists), []).
:- use_module(library(os), []).
:- use_module(library(pio), []).
:- use_module(library(si), []).

% swi specific
goal_expansion(use_foreign_library(_), true).

goal_expansion(compound_name_arguments(Compound, Name, Args), (Compound =.. [Name | Args])).

goal_expansion(term_to_atom(Term, Atom), sys:term_to_atom_helper(Term, Atom)).

:- meta_predicate(ignore(0)).
ignore(_) :- not_used.
goal_expansion(ignore(Goal), sys:ignore_helper(Module:Goal)) :- prolog_load_context(module, Module).

goal_expansion(opt_arguments(Spec, Opts, PosArgs), sys:opt_arguments_helper(Spec, Opts, PosArgs)).
goal_expansion(opt_help(Spec, Help), sys:opt_help_helper(Spec, Help)).

% TODO implement on_signal equivalent in scryer-prolog
goal_expansion(on_signal(_Sig, _Old, _New), true).

% between

goal_expansion(between(Min, Max, Val), sys:between_helper(Min, Max, Val)).

% charsio

goal_expansion(char_type(Char, Cat), sys:char_type_helper(Char, Cat)).

% error
goal_expansion(call_with_error_ctx(Goal, Ctx), error:call_with_error_context(Module:Goal, Ctx)) :- prolog_load_context(module, Module).

% files
goal_expansion(exists_file(AtomPath), (atom_chars(AtomPath, Path), files:file_exists(Path))).

% format 
writeln(_) :- not_used.
writeln(_,_) :- not_used.

goal_expansion(writeln(String), sys:writeln_helper(String)).
goal_expansion(writeln(Stream, String), sys:writeln_helper(Stream, String)).

goal_expansion(compat_format(Fmt, Args), iso_ext:format(Fmt, Args)).
goal_expansion(compat_format(Stream, Fmt, Args), sys:format_helper(Stream, Fmt, Args)).

%iso_ext
:- meta_predicate(compat_forall(0,0)).
compat_forall(_,_) :-not_used.
goal_expansion(compat_forall(Goal, Test), sys:forall_helper(Module:Goal, Module:Test)) :- prolog_load_context(module, Module).

goal_expansion(nb_getval(Var, Val), sys:nb_getval_helper(Var, Val)).
goal_expansion(nb_setval(Var, Val), iso_ext:bb_put(Var, Val)).
goal_expansion(nb_current(Var, Val), sys:nb_current_helper(Var, Val)).
goal_expansion(nb_delete(Var), sys:nb_delete_helper(Var)).

% lists
goal_expansion(string(Term), si:chars_si(Term)).

goal_expansion(is_list(Xs), si:list_si(Xs)).

goal_expansion(is_set(Xs), sys:is_set_helper(Xs)).

goal_expansion(last(List, Last), lists:append(_, [Last], List)).
goal_expansion(nexto(X, Y, List), lists:append(_, [X, Y | _], List)).

goal_expansion(selectchk(Elem, List, Rest), once(select(Elem, List, Rest))).

goal_expansion(atom_string(Atom, Chars), sys:atom_string_helper(Atom, Chars)).

goal_expansion(sub_string(String, Before, Len, After, SubString), sys:sub_string_helper(String, Before, Len, After, SubString)).

goal_expansion(string_chars(String, Chars), (String = Chars)).
goal_expansion(string_concat(A, B, C), lists:append(A, B, C)).

goal_expansion(delete(Xs0, Elem, Xs1), sys:delete_helper(Xs0, Elem, Xs1)).

:- meta_predicate(max_member(2,-,+)).
max_member(_,_,_) :- not_used.
goal_expansion(max_member(Pred, Max, List), sys:max_member_helper(Module:Pred, Max, List)) :- prolog_load_context(module, Module).


% os

goal_expansion(getenv(Key, Value), sys:getenv_helper(Key, Value)).

goal_expansion(shell(Command), sys:shell_helper(Command)).
goal_expansion(shell(Command, ExitCode), sys:shell_helper(Command, ExitCode)).

% 
:- initialization((
   compat_format(string(Str), "scryer/init.pl initialized", []),
    writeln(Str)
)).