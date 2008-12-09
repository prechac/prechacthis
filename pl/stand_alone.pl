:- ['prechacthis.pl'].
%:- ['../xpce/server_gui.pl'].

build_prechacthis :-
	build_prechacthis('prechacthis').

build_prechacthis(Target) :-
	make,
	recorda(prechacthis_server_type, resource),
	qsave_program(Target, [stand_alone(true), class(runtime), autoload(true), goal(server(4211))]).
	

