:- use_module(library(pce)).

	
server_gui_stop :-
	new(D, dialog('PrechacThis')),
	send(D, append,
	     button(stop_PrechacThis, message(D, return, stop))),
	send(D, default_button, stop_PrechacThis),
	get(D, confirm, _Rval),
	free(D).

server_gui :-
	server,
	server_gui_stop,
	fail, !.
server_gui :-
	server_stop,
	halt, !.
server_gui :-
	halt, !.