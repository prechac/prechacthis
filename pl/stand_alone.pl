:- ['prechacthis.pl'].
%:- ['../xpce/server_gui.pl'].


build_prechacthis(Type) :-
	make,
	qsave_program('building/linux/prechacthis', [stand_alone(true), class(runtime), autoload(true), goal(server)]).
	

