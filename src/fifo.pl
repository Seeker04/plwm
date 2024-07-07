% MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

:- module(fifo, []).

setup_fifo() :-
	optcnf_then(fifo_enabled(true), optcnf_then(fifo_path(FifoPath), (
		catch(delete_file(FifoPath), _, true), % cleanup from previous execution
		string_concat("mkfifo ", FifoPath, MkFifoCmd), % didn't find an swipl predicate for this
		shell(MkFifoCmd, ExitCode),
		(ExitCode == 0 ->
			thread_create(fifo:process_fifo(FifoPath), _, [detached(true)])
		; true)
	)))
.

process_fifo(FifoPath) :-
	open(FifoPath, read, Fifo),
	(read_terms(Fifo, Jobs) ->
		jobs_notify(Jobs)
	; true),
	close(Fifo),
	process_fifo(FifoPath)
.

read_terms(S, Terms) :-
	read_terms_(S, [], TermsR),
	reverse(TermsR, Terms)
.
read_terms_(S, AccTerms, Terms) :-
	(catch(read(S, Term), Ex, (writeln(Ex), fail)) ->
		(Term = end_of_file ->
			Terms = AccTerms
		;
			read_terms_(S, [Term|AccTerms], Terms)
		)
	)
.

