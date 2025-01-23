% MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

% helpers
plus2(N, M) :- M is N + 2.
times3(N, M) :- M is N * 3.

:- begin_tests(utils_tests).

:- use_module("../src/utils").

test("valid_callable +") :-
	assertion(utils:valid_callable(true)),
	assertion(utils:valid_callable(ignore(false))),
	assertion(utils:valid_callable(utils:is_float(0.0))),
	assertion(utils:valid_callable(utils:valid_callable(true)))
.

test("valid_callable - (non-existing predicate)") :-
	assertion(\+ utils:valid_callable(i_dont_exist)),
	assertion(\+ utils:valid_callable(i_dont_exist(foo))),
	assertion(\+ utils:valid_callable(i_dont_exist(foo, bar)))
.

test("valid_callable - (incorrect module specified)") :-
	assertion(\+ utils:valid_callable(wrong_module:is_float(0.0)))
.

test("valid_callable - (arity mismatch)") :-
	assertion(\+ utils:valid_callable(plus(2, 3))), % 3rd arg missing
	assertion(\+ utils:valid_callable(utils:is_float(0.0, 1.0))) % extra arg
.

test("bool_negated +") :-
	assertion(utils:bool_negated(true, false)),
	assertion(utils:bool_negated(false, true))
.
test("bool_negated -") :-
	assertion(\+ utils:bool_negated(true, true)),
	assertion(\+ utils:bool_negated(false, false))
.

test("is_float +") :-
	assertion(utils:is_float(0.0)),
	assertion(utils:is_float(-0.0)),
	assertion(utils:is_float(3.1415)),
	assertion(utils:is_float(-3.1415)),
	assertion(utils:is_float(1/3)),
	assertion(utils:is_float(0/3)),
	assertion(utils:is_float(-1/3)),
	assertion(utils:is_float(-0/3))
.
test("is_float -") :-
	assertion(\+ utils:is_float(0)),
	assertion(\+ utils:is_float(1)),
	assertion(\+ utils:is_float(-1)),
	assertion(\+ utils:is_float(foo)),
	assertion(\+ utils:is_float("bar"))
.

test("dumpstack", [cleanup(delete_file("/tmp/plwm_stack.log"))]) :-
	utils:dumpstack,
	open("/tmp/plwm_stack.log", read, File),
	read_string(File, FileSize, _),
	assertion(0 < FileSize),
	close(File)
.

test("startswith_char +") :-
	assertion(utils:startswith_char([f], f)),
	assertion(utils:startswith_char([f, _, _], f))
.
test("startswith_char -") :-
	assertion(\+ utils:startswith_char([], _))
.

test("ensure_ampersand") :-
	assertion(utils:ensure_ampersand("", "&")),
	assertion(utils:ensure_ampersand("&", "&")),
	assertion(utils:ensure_ampersand("foo", "foo&")),
	assertion(utils:ensure_ampersand("foo&", "foo&"))
.

test("shellcmd", [cleanup(delete_file(TestFile))]) :-
	TestFile = "/tmp/test-date-output",
	string_concat("date > ", TestFile, ValidCmd),
	string_concat("dateee 2>/dev/null > ", TestFile, InvalidCmd),

	shellcmd(ValidCmd),
	sleep(0.1), % cmd executes in bg, wait for the file to be written
	assertion((size_file(TestFile, Size), 0 < Size)),

	shellcmd(InvalidCmd),
	sleep(0.1),
	assertion(size_file(TestFile, 0))
.

test("warn_invalid_arg", [cleanup(delete_file(ErrorFile))]) :-
	ErrorFile = "/tmp/test-errormsg",

	open(ErrorFile, write, ErrorOut),
	assertion(with_output_to(ErrorOut, utils:warn_invalid_arg(foo, bar), [capture([user_error])])),
	close(ErrorOut),

	open(ErrorFile, read, ErrorIn),
	read_string(ErrorIn, _, Str),
	assertion(Str == "warning: invalid argument to foo: bar, ignored\n"),
	close(ErrorIn)
.

test("split_at +") :-
	assertion(utils:split_at(0, L, [], L)),
	assertion(utils:split_at(1, [A|L], [A], L)),
	assertion(utils:split_at(2, [A, B|L], [A, B], L))
.

test("split_at -") :-
	assertion(\+ utils:split_at(1, [], _, _)),
	assertion(\+ utils:split_at(1, _, [], _)),
	assertion(\+ utils:split_at(2, [], _, _)),
	assertion(\+ utils:split_at(2, [_], _, _)),
	assertion(\+ utils:split_at(2, _, [], _)),
	assertion(\+ utils:split_at(2, _, [_], _))
.

test("pair_keys_with +") :-
	assertion(utils:pair_keys_with([], [], _)),
	assertion(utils:pair_keys_with([K1-V], [K1], V)),
	assertion(utils:pair_keys_with([K1-V, K2-V], [K1, K2], V)),
	assertion(utils:pair_keys_with([K1-V, K2-V, K3-V], [K1, K2, K3], V))
.

test("pair_keys_with -") :-
	assertion(\+ utils:pair_keys_with([], [_|_], _)),
	assertion(\+ utils:pair_keys_with([_|_], [], _)),
	assertion(\+ (utils:pair_keys_with([K-V], [J], V), K \= J)),
	assertion(\+ (utils:pair_keys_with([K-V], [K], W), V \= W))
.

test("pair_values_with +") :-
	assertion(utils:pair_values_with([], _, [])),
	assertion(utils:pair_values_with([K-V1], [V1], K)),
	assertion(utils:pair_values_with([K-V1, K-V2], [V1, V2], K)),
	assertion(utils:pair_values_with([K-V1, K-V2, K-V3], [V1, V2, V3], K))
.

test("pair_values_with -") :-
	assertion(\+ utils:pair_values_with([], [_|_], _)),
	assertion(\+ utils:pair_values_with([_|_], [], _)),
	assertion(\+ (utils:pair_values_with([K-V], [V], J), K \= J)),
	assertion(\+ (utils:pair_values_with([K-V], [W], K), V \= W))
.

test("list_to_oldassoc +") :-
	assertion(utils:list_to_oldassoc([], Assoc, Assoc)),

	empty_assoc(Assoc1),

	utils:list_to_oldassoc([], Assoc1, Assoc2),
	assertion(is_assoc(Assoc2)),
	assertion(empty_assoc(Assoc2)),

	utils:list_to_oldassoc([k1-v1], Assoc2, Assoc3),
	assertion(is_assoc(Assoc3)),
	assertion(assoc_to_keys(Assoc3, [k1])),
	assertion(assoc_to_values(Assoc3, [v1])),
	assertion(assoc_to_list(Assoc3, [k1-v1])),

	utils:list_to_oldassoc([k2-v2, k3-v3], Assoc3, Assoc4),
	assertion(is_assoc(Assoc4)),
	assertion(assoc_to_keys(Assoc4, [k1, k2, k3])),
	assertion(assoc_to_values(Assoc4, [v1, v2, v3])),
	assertion(assoc_to_list(Assoc4, [k1-v1, k2-v2, k3-v3]))
.

test("list_to_oldassoc +") :-
	assertion(\+ utils:list_to_oldassoc(k, _, _)),
	assertion(\+ utils:list_to_oldassoc([k], _, _)),
	empty_assoc(Assoc),
	assertion(\+ utils:list_to_oldassoc([k-v, w], Assoc, _))
.

test("global_key_value +", [
	setup((
		list_to_assoc([k1-v1, k2-v2], A), nb_setval(g, A)
	)),
	cleanup(
		nb_delete(g)
	)
]) :-
	assertion(utils:global_key_value(g, k1, v1)),
	assertion(utils:global_key_value(g, k2, v2))
.

test("global_key_value -", [
	setup((
		empty_assoc(A), nb_setval(g1, A),
		list_to_assoc([k1-v1], B), nb_setval(g2, B)
	)),
	cleanup((
		nb_delete(g1), nb_delete(g2)
	))
]) :-
	assertion(\+ utils:global_key_value(g1, _, _)),
	assertion(\+ utils:global_key_value(g2, k2, _)),
	assertion(\+ (utils:global_key_value(g2, k1, V), V \= v1))
.

test("global_key_newvalue +", [
	setup((
		empty_assoc(EA), nb_setval(g1, EA),
		list_to_assoc([k1-v1, k2-v2], A), nb_setval(g2, A)
	)),
	cleanup((
		nb_delete(g1), nb_delete(g2)
	))
]) :-
	assertion(utils:global_key_newvalue(g1, k1, v1)),
	nb_getval(g1, A1),
	assertion(is_assoc(A1)),
	assertion(assoc_to_list(A1, [k1-v1])),

	assertion(utils:global_key_newvalue(g2, k1, v1new)),
	nb_getval(g2, A2),
	assertion(is_assoc(A2)),
	assertion(assoc_to_list(A2, [k1-v1new, k2-v2])),

	assertion(utils:global_key_newvalue(g2, k2, v2new)),
	nb_getval(g2, A3),
	assertion(is_assoc(A3)),
	assertion(assoc_to_list(A3, [k1-v1new, k2-v2new])),

	assertion(utils:global_key_newvalue(g2, k3, v3)),
	nb_getval(g2, A4),
	assertion(is_assoc(A4)),
	assertion(assoc_to_list(A4, [k1-v1new, k2-v2new, k3-v3]))
.

test("global_key_newvalue - (nonexistent_g)", [throws(_)]) :-
	utils:global_key_newvalue(nonexistent_g, _, _)
.

test("global_key_newvalue - (nonassoc_g)", [
	setup(
		nb_setval(nonassoc_g, 42)
	),
	cleanup(
		nb_delete(nonassoc_g)
	),
	throws(_)
]) :-
	utils:global_key_newvalue(nonassoc_g, _, _)
.

test("global_value +", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, WsA),
		nb_setval(active_mon, amon),
		nb_setval(active_ws, WsA),
		list_to_assoc([amon-aws-v], A), nb_setval(g, A)
	)),
	cleanup((
		nb_delete(active_mon), nb_delete(active_ws), nb_delete(g)
	))
]) :-
	assertion(utils:global_value(g, v))
.

test("global_value - (no active_mon)", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, WsA),
		nb_setval(active_ws, WsA),
		list_to_assoc([amon-aws-v], A), nb_setval(g, A)
	)),
	cleanup((
		nb_delete(active_ws), nb_delete(g)
	)),
	throws(_)
]) :-
	utils:global_value(g, v)
.

test("global_value - (no active_ws)", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, _),
		nb_setval(active_mon, amon),
		list_to_assoc([amon-aws-v], A), nb_setval(g, A)
	)),
	cleanup((
		nb_delete(active_mon), nb_delete(g)
	)),
	throws(_)
]) :-
	utils:global_value(g, v)
.

test("global_value - (nonexistent_g)", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, WsA),
		nb_setval(active_mon, amon),
		nb_setval(active_ws, WsA),
		list_to_assoc([amon-aws-v], A), nb_setval(g, A)
	)),
	cleanup((
		nb_delete(active_mon), nb_delete(active_ws), nb_delete(g)
	)),
	throws(_)
]) :-
	utils:global_value(nonexistent_g, _)
.

test("global_value - (wrong value)", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, WsA),
		nb_setval(active_mon, amon),
		nb_setval(active_ws, WsA),
		list_to_assoc([amon-aws-v], A), nb_setval(g, A)
	)),
	cleanup((
		nb_delete(active_mon), nb_delete(active_ws), nb_delete(g)
	))
]) :-
	assertion(\+ (utils:global_value(g, V), V \= v))
.

test("global_newvalue +", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, WsA),
		nb_setval(active_mon, amon),
		nb_setval(active_ws, WsA),
		list_to_assoc([amon-aws-v1], A), nb_setval(g, A)
	)),
	cleanup((
		nb_delete(active_mon), nb_delete(active_ws), nb_delete(g)
	))
]) :-
	assertion(utils:global_newvalue(g, v2)),
	nb_getval(g, G1),
	assertion(is_assoc(G1)),
	assertion(assoc_to_list(G1, [amon-aws-v2])),

	assertion(utils:global_newvalue(g, v3)),
	nb_getval(g, G2),
	assertion(is_assoc(G2)),
	assertion(assoc_to_list(G2, [amon-aws-v3]))
.

test("global_newvalue - (nonexistent_g)", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, WsA),
		nb_setval(active_mon, amon),
		nb_setval(active_ws, WsA),
		list_to_assoc([amon-aws-v1], A), nb_setval(g, A)
	)),
	cleanup((
		nb_delete(active_mon), nb_delete(active_ws), nb_delete(g)
	)),
	throws(_)
]) :-
	utils:global_newvalue(nonexistent_g, _)
.

test("global_newvalue - (nonassoc_g)", [
	setup((
		empty_assoc(EA),
		put_assoc(amon, EA, aws, WsA),
		nb_setval(active_mon, amon),
		nb_setval(active_ws, WsA),
		nb_setval(g, 42)
	)),
	cleanup((
		nb_delete(active_mon), nb_delete(active_ws), nb_delete(g)
	)),
	throws(_)
]) :-
	utils:global_newvalue(nonassoc_g, _)
.

test("swap_with_prev +") :-
	assertion(utils:swap_with_prev([A, B|_], B, [B, A|_])),
	assertion(utils:swap_with_prev([A, B, C|_], B, [B, A, C|_])),
	assertion(utils:swap_with_prev([A, B, C|_], C, [A, C, B|_]))
.

test("swap_with_prev -") :-
	assertion(\+ utils:swap_with_prev([], _, _)),
	assertion(\+ utils:swap_with_prev([_], _, _)),
	assertion(\+ utils:swap_with_prev([a, b], a, _)),
	assertion(\+ utils:swap_with_prev([a, b], c, _)),
	assertion(\+ (utils:swap_with_prev([A, B], B, [C, D]), C-D \= B-A))
.

test("swap_with_next +") :-
	assertion(utils:swap_with_next([A, B|_], A, [B, A|_])),
	assertion(utils:swap_with_next([A, B, C|_], A, [B, A, C|_])),
	assertion(utils:swap_with_next([A, B, C|_], B, [A, C, B|_]))
.

test("swap_with_next -") :-
	assertion(\+ utils:swap_with_next([], _, _)),
	assertion(\+ utils:swap_with_next([_], _, _)),
	assertion(\+ utils:swap_with_next([a, b], b, _)),
	assertion(\+ utils:swap_with_next([a, b], c, _)),
	assertion(\+ (utils:swap_with_next([A, B], A, [C, D]), C-D \= B-A))
.

test("n_item_clones +") :-
	assertion(utils:n_item_clones(0, _, [])),
	assertion(utils:n_item_clones(1, A, [A])),
	assertion(utils:n_item_clones(2, A, [A, A])),
	assertion(utils:n_item_clones(3, A, [A, A, A]))
.

test("n_item_clones -") :-
	assertion(\+ utils:n_item_clones(-1, _, _)),
	assertion(\+ (utils:n_item_clones(0, _, L), \+ L = [])),
	assertion(\+ (utils:n_item_clones(1, _, L), \+ length(L, 1))),
	assertion(\+ (utils:n_item_clones(2, _, L), \+ length(L, 2))),
	assertion(\+ (utils:n_item_clones(3, _, L), \+ length(L, 3))),

	assertion(\+ (utils:n_item_clones(2, A, [A, B]), A \= B)),
	assertion(\+ (utils:n_item_clones(3, A, [A, B, C]), B-C \= A-A))
.

test("n_step_list +") :-
	assertion(utils:n_step_list(1, _, [_])),

	assertion(utils:n_step_list(2, plus2, [1, 3])),
	assertion(utils:n_step_list(3, plus2, [1, 3, 5])),
	assertion(utils:n_step_list(4, plus2, [1, 3, 5, 7])),

	assertion(utils:n_step_list(2, times3, [1, 3])),
	assertion(utils:n_step_list(3, times3, [1, 3, 9])),
	assertion(utils:n_step_list(4, times3, [1, 3, 9, 27]))
.

test("n_step_list -") :-
	assertion(\+ utils:n_step_list(-1, _, _)),
	assertion(\+ utils:n_step_list(0, _, _)),
	assertion(\+ (utils:n_step_list(1, _, L), length(L, N), N \= 1))
.

test("alternate_merge +") :-
	assertion(utils:alternate_merge( []  , []  , []     )),
	assertion(utils:alternate_merge( [A] , []  , [A]    )),
	assertion(utils:alternate_merge( []  , [A] , [A]    )),
	assertion(utils:alternate_merge( [A] , [A] , [A, A] )),
	assertion(utils:alternate_merge( [A] , [B] , [A, B] )),
	assertion(utils:alternate_merge( [B] , [A] , [B, A] )),

	assertion(utils:alternate_merge( [A, B, C] , []     , [A, B, C]       )),
	assertion(utils:alternate_merge( [A, B, C] , [X]    , [A, X, B, C]    )),
	assertion(utils:alternate_merge( [A, B, C] , [X, Y] , [A, X, B, Y, C] )),

	assertion(utils:alternate_merge( []     , [X, Y, Z] , [X, Y, Z]       )),
	assertion(utils:alternate_merge( [A]    , [X, Y, Z] , [A, X, Y, Z]    )),
	assertion(utils:alternate_merge( [A, B] , [X, Y, Z] , [A, X, B, Y, Z] )),

	assertion(utils:alternate_merge( [A, B, C] , [X, Y, Z] , [A, X, B, Y, C, Z] ))
.

test("alternate_merge -") :-
	assertion(\+ utils:alternate_merge( []    , []    , [_|_] )),
	assertion(\+ utils:alternate_merge( [_|_] , []    , []    )),
	assertion(\+ utils:alternate_merge( []    , [_|_] , []    )),

	assertion(\+ (utils:alternate_merge( [A] , []  , [B])    , A \= B )),
	assertion(\+ (utils:alternate_merge( []  , [A] , [B])    , A \= B )),
	assertion(\+ (utils:alternate_merge( [A] , [B] , [B, A]) , A \= B ))
.

test("pair_up_lists +") :-
	assertion(utils:pair_up_lists( []        , []        , []              )),
	assertion(utils:pair_up_lists( [a]       , [x]       , [a-x]           )),
	assertion(utils:pair_up_lists( [a, b]    , [x, y]    , [a-x, b-y]      )),
	assertion(utils:pair_up_lists( [a, b, c] , [x, y, z] , [a-x, b-y, c-z] ))
.

test("pair_up_lists -") :-
	assertion(\+ utils:pair_up_lists( []     , []     , [_|_] )),
	assertion(\+ utils:pair_up_lists( []     , [_|_]  , _     )),
	assertion(\+ utils:pair_up_lists( [_|_]  , []     , _     )),
	assertion(\+ utils:pair_up_lists( [_, _] , [_]    , _     )),
	assertion(\+ utils:pair_up_lists( [_]    , [_, _] , _     )),
	assertion(\+ (utils:pair_up_lists([A], [X], [A-X] ), A \= X))
.

test("str_withoutlastch +") :-
	assertion(utils:str_withoutlastch("a", "")),
	assertion(utils:str_withoutlastch("aa", "a")),
	assertion(utils:str_withoutlastch("aaa", "aa")),
	assertion(utils:str_withoutlastch("abcd", "abc")),

	assertion(utils:str_withoutlastch("\n", "")),
	assertion(utils:str_withoutlastch("\n\n", "\n")),
	assertion(utils:str_withoutlastch("abc\n", "abc")),
	assertion(utils:str_withoutlastch("abc\nd", "abc\n"))
.

test("str_withoutlastch -") :-
	assertion(\+ (utils:str_withoutlastch("a", Str), string_length(Str, N), N \= 0)),
	assertion(\+ (utils:str_withoutlastch("ab", Str), string_length(Str, N), N \= 1)),
	assertion(\+ (utils:str_withoutlastch("abc", Str), string_length(Str, N), N \= 2))
.

test("str_withoutlastch - (empty input)", [throws(_)]) :-
	utils:str_withoutlastch("", _)
.

:- end_tests(utils_tests).

