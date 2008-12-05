:- ['prechacthis.pl'].
%:- ['../xpce/server_gui.pl'].


build_prechacthis(Type) :-
	make,
	build_prechacthis_goal(Type, Goal),
	qsave_program('building/linux/prechacthis', [stand_alone(true), class(runtime), autoload(true), goal(Goal)]).
	
build_prechacthis_goal(gui, server_gui) :- !.
build_prechacthis_goal(_, server) :- !.

:- build_prechacthis(no_gui), halt.
