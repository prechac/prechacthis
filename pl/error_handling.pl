initErrorHandling :- 
	retractall(prechac_error(_)),!.
	
error(String) :-
	asserta(prechac_error(String)),
	!,fail.