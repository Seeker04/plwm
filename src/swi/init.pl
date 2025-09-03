:- set_prolog_flag(autoload, false).

% enabling this causes compilation to fail with the following cryptic error
% ERROR: '$open_wic'/2: Type error: `option' expected, found `init_file(none)' (a compound)
% :- set_prolog_flag(iso, true).

goal_expansion(format_string(Fmt, Args, Str), format(string(Str), Fmt, Args)).