#!/opt/local/bin/swipl -f none -g main -s


%%	main
%
%	Start the PrechacThis server and wait. Does not provide access
%	to the toplevel, so it can be run as a background process.
%	
%	Start as
%	
%	    ==
%	    ./prechacthis_server.pl  [--daemon] [--port=Port] [--workers=Workers] [--servertype=Type] [--verbos]
%	    ==


main :-
	current_prolog_flag(argv, Argv),
	set_working_directory(Argv),
	append(_, [--|AV], Argv), !,
	start_server(AV),
	wait(AV).
	

start_server(Argv) :-
	av_option(port(Port), Argv, 4211),
	av_option(workers(Workers), Argv, 3),
	av_option(servertype(Type), Argv, file),
	av_option(verbos(Verbos), Argv),
	recorda(prechacthis_server_type, Type),
	(Verbos = false ->
		set_prolog_flag(verbose_load, false);
		set_prolog_flag(verbose_load, true)
	),
	use_module('pl/prechacthis'),
	server(Port, Workers).
	
set_working_directory(Argv) :-
	member(File, Argv),
	atom_concat(Dir, 'prechacthis_server.pl', File),
	working_directory(_Old, Dir), !.	
set_working_directory(_).
	
wait(Argv) :-
	memberchk('--daemon', Argv), !,
	thread_get_message(_),
	halt.
wait(_).

av_option(Option, Argv, Default) :-
	Option =.. [Name,Value],
	(   format(atom(Prefix), '--~w=', [Name]),
	    member(Av, Argv),
	    atom_concat(Prefix, ValueAtom, Av),
	    atom_codes(ValueAtom, Codes),
	    name(Value0, Codes)
	->  Value = Value0
	;   Value = Default
	).
	
av_option(Option, Argv) :-
	Option =.. [Name, Value],
	(   format(atom(Av), '--~w', [Name]),
		member(Av, Argv)
	->  Value = true
	;   Value = false
	).
