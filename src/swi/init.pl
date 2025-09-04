:- set_prolog_flag(autoload, false).

% enabling this causes compilation to fail with the following cryptic error
% ERROR: '$open_wic'/2: Type error: `option' expected, found `init_file(none)' (a compound)
% :- set_prolog_flag(iso, true).

goal_expansion(compat_format(Fmt, Args), format(Fmt, Args)).
goal_expansion(compat_format(Stream, Fmt, Args), format(Stream, Fmt, Args)).

:- meta_predicate(compat_forall(0,0)).
compat_forall(_,_) :-not_used.

%iso_ext
goal_expansion(compat_forall(Goal, Test), forall(Module:Goal, Module:Test)) :- prolog_load_context(module, Module).