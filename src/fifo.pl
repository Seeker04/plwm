% MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

:- module(fifo, []).

%! setup_fifo is det
%
%  If fifo_enabled/1 and fifo_path/1 are set, attempts to create
%  a named pipe with mkfifo(1).
%  If the fifo is created, its path is passed to fifo:process_fifo/1 on a detached thread.
setup_fifo :-
	(user:fifo_enabled(true), user:fifo_path(FifoPath) ->
		catch(delete_file(FifoPath), _, true), % cleanup from previous execution
		string_concat("mkfifo ", FifoPath, MkFifoCmd), % no swipl predicate for this
		shell(MkFifoCmd, ExitCode),
		(ExitCode == 0 ->
			thread_create(fifo:process_fifo(FifoPath), _, [detached(true)])
		; writeln(user_error, "Could not spawn command fifo!"))
	; true)
.

%! process_fifo(++FifoPath:string) is det
%
%  Opens the command fifo, reads all available terms from it into "jobs",
%  notifies the main thread about the pending jobs, who executes them,
%  finally, the fifo is closed and process_fifo waits for new input in the
%  next recursive call.
%
%  Note: opening and closing the file in each recursive call is needed, otherwise
%        read_terms/2 would keep returning empty lists in a non-blocking endless loop.
%
%  @arg FifoPath file path to the command fifo
process_fifo(FifoPath) :-
	open(FifoPath, read, Fifo),
	(read_terms(Fifo, Jobs) ->
		jobs_notify(Jobs)
	; true),
	close(Fifo),
	process_fifo(FifoPath)
.

%! read_terms(++S:stream, --Terms:[term]) is semidet
%
%  Reads content from an already open input stream to a list of terms.
%  Fails if some content cannot be parsed as a term.
%
%  @arg S input file stream that has been opened prior to this call
%  @arg Terms list of terms read from the stream
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
