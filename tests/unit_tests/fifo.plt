% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

% mocks
fifo_enabled(true).
fifo_path("/tmp/test-fifo").

:- begin_tests(fifo_tests).

:- use_module("../../src/fifo").

test("setup_fifo +", [
	setup(
		fifo_path(FifoPath)
	),
	cleanup(
		delete_file(FifoPath)
		% Note: thread spawned by setup_fifo is detached, so no need to join
	)
]) :-
	assertion(fifo:setup_fifo),
	assertion(access_file(FifoPath, read)),

	% Check if setup_fifo started the thread for process_fifo,
	% i.e. an extra thread is running next to 'main' and 'gc'
	findall(T, thread_property(T, _), Threads),
	assertion(list_to_set(Threads, [main, gc, _]))
.

test("setup_fifo + (already exists)", [
	setup((
		fifo_path(FifoPath),
		open(FifoPath, write, _) % create empty file
	)),
	cleanup(
		delete_file(FifoPath)
	)
]) :-
	assertion(fifo:setup_fifo),
	assertion(access_file(FifoPath, read)),

	findall(T, thread_property(T, _), Threads),
	assertion(list_to_set(Threads, [main, gc, _, _]))
	% Note: one more thread due to execution of prior testcase,
	% unfortunately we cannot destroy detached threads:/
.

test("process_fifo", [
	setup((
		fifo_path(FifoPath),
		string_concat("mkfifo ", FifoPath, MkFifoCmd),
		shell(MkFifoCmd)
	)),
	cleanup(
		delete_file(FifoPath)
	)
]) :-
	string_concat("sleep .1; echo 't1.' >> ", FifoPath, S),
	string_concat(S, " &", WriteFifoCmd),
	shell(WriteFifoCmd),
	assertion(call_with_depth_limit(fifo:process_fifo(FifoPath), 2, _))
.

test("read_terms +", [
	setup((
		TestFile = "/tmp/test-terms",
		open(TestFile, write, SWrite),
		writeln(SWrite, "t1. t2(a1,   a2).\nt3(a1,\n\nc1( a2\t, a3),c2(c3(a4),  a5) ). t4(\na6)."),
		close(SWrite),
		open(TestFile, read, SRead)
	)),
	cleanup((
		close(SRead),
		delete_file(TestFile)
	))
]) :-
	assertion(fifo:read_terms(SRead, [
		t1,
		t2(a1, a2),
		t3(a1, c1(a2, a3), c2(c3(a4), a5)),
		t4(a6)
	]))
.

test("read_terms -", [
	setup((
		TestFile = "/tmp/test-terms",
		open(TestFile, write, SWrite),
		writeln(SWrite, "t1("),
		close(SWrite),
		open(TestFile, read, SRead)
	)),
	cleanup((
		close(SRead),
		delete_file(TestFile)
	))
]) :-
	assertion(\+ fifo:read_terms(SRead, _))
.

:- end_tests(fifo_tests).

