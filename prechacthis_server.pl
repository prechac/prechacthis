#!/opt/local/bin/swipl -f none -g main -s


%%	main
%
%	Start the PrechacThis server and wait. Does not provide access
%	to the toplevel, so it can be run as a background process.
%	
%	Start as
%	
%	    ==
%	    ./prechacthis_server.pl  [--daemon] [--port=Port] [--workers=Workers] [--servertype=Type]
%	    ==


main :-
	current_prolog_flag(argv, Argv),
	append(_, [--|AV], Argv), !,
	start_server(AV),
	wait(AV).
	

start_server(Argv) :-
	av_option(port(Port), Argv, 4211),
	av_option(workers(Workers), Argv, 3),
	av_option(servertype(Type), Argv, file),
	recorda(prechacthis_server_type, Type),
	consult('pl/prechacthis'),
	server(Port, Workers).
	
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
