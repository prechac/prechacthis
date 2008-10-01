format_href(Href) :-
	href_type(HrefType),
	format_href(Href, HrefType).
format_href(Href, ajax) :-
	format("'javascript:loadContent(\"~w\");'", [Href]), !.
format_href(Href, _Html) :-
	format("~w", [Href]).
	
if_ajax(Call, Type) :-
	(Type = ajax ->
		call(Call);
		true
	).
	