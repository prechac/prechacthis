
format_href(Href) :-
	href_type(ajax), !,
	format_href(Href, ajax).
format_href(Href) :-
	format_href(Href, html).
	
format_href(Href, ajax) :-
	format("'javascript:loadContent(\"~w\");'", [Href]), !.
format_href(Href, html) :-
	format("'~w'", [Href]).
	
format_href(Pattern, NumberOfJugglers, SwapList, BackURL) :-
	pattern_to_string(Pattern, PatternStr),
	list_to_string(SwapList, SwapListStr),
	format(atom(Href), "./info.php?pattern=~s&amp;persons=~w&amp;swap=~s&amp;back=~w", [PatternStr, NumberOfJugglers, SwapListStr, BackURL]),
	format_href(Href).
	
if_ajax(Call, Type) :-
	(Type = ajax ->
		call(Call);
		true
	).
	