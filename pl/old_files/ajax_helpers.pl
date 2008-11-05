
format_href(Href) :-
	href_type(ajax), !,
	format_href(Href, ajax).
format_href(Href) :-
	format_href(Href, html).
	
format_href(Href, ajax) :-
	format("'javascript:loadContent(\"~w\");'", [Href]), !.
format_href(Href, html) :-
	format("'~w'", [Href]).
	
format_href(Pattern, Persons, SwapList, BackURL) :-
	a2Atom(Pattern, PatternStr),
	a2Atom(SwapList, SwapListStr),
	format(atom(Href), "./info.php?pattern=~w&amp;persons=~w&amp;swap=~w&amp;back=~w", [PatternStr, Persons, SwapListStr, BackURL]),
	format_href(Href).
	
	
	
	
	
	
if_ajax(Call, Type) :-
	(Type = ajax ->
		call(Call);
		true
	).
	