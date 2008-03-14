initErrorHandling :- 
	retractall(prechac_error(_)),!.
	
raise_error(String) :-
	asserta(prechac_error(String)),
	!,fail.
	
write_errors(Format) :-
	forall(prechac_error(Error), format(Format, [Error])).