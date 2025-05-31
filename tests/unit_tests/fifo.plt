% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

% mocks
optcnf_then(fifo_enabled(true), Then) :- Then.
optcnf_then(fifo_path(FifoPath), Then) :- FifoPath = "/tmp/test-fifo", Then.

:- begin_tests(fifo_tests).

:- use_module("../../src/fifo").

test("setup_fifo +", [
	setup((
		optcnf_then(fifo_path(FifoPath), true),
		fifo:setup_fifo(FifoThread)
	)),
	cleanup((
		thread_send_message(FifoThread, shutdown),
		thread_join(FifoThread),
		delete_file(FifoPath)
	))
]) :-
	assertion(access_file(FifoPath, read)),
	assertion(thread_property(FifoThread, status(running)))
.

test("setup_fifo + (already exists)", [
	setup((
		optcnf_then(fifo_path(FifoPath), true),
		open(FifoPath, write, Stream), % create empty file
		fifo:setup_fifo(FifoThread)
	)),
	cleanup((
		close(Stream),
		thread_send_message(FifoThread, shutdown),
		thread_join(FifoThread),
		delete_file(FifoPath)
	))
]) :-
	assertion(access_file(FifoPath, read)),
	assertion(thread_property(FifoThread, status(running)))
.

test("process_fifo", [
	setup((
		optcnf_then(fifo_path(FifoPath), true),
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

