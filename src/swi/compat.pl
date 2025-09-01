:- module(compat, [
    error/2, 
    format_string/3
]).


error(Exception, Src) :- throw(error(Exception, Src)).

format_string(FmtStr, Args, Str) :- format(chars(Str), FmtStr, Args).

