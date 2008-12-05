:- ['prechacthis.pl'].
%:- ['../xpce/server_gui.pl'].

build_prechacthis :-
	build_prechacthis('prechacthis').

build_prechacthis(Target) :-
	make,
	qsave_program(Target, [stand_alone(true), class(runtime), autoload(true), goal(server)]).
	

