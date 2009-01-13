:- use_module('prechacthis').
%:- ['../xpce/server_gui.pl'].

:- recorda(prechacthis_server_type, resource).

build_prechacthis :-
	build_prechacthis('prechacthis').

build_prechacthis(Target) :-
	make,
	recorda(prechacthis_server_type, resource),
	qsave_program(Target, [stand_alone(true), class(runtime), autoload(true), goal(server(4211, 2))]).
	

