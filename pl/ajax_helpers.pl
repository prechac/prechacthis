format_href(Href) :-
	href_type(HrefType),
	format_href(Href, HrefType).
format_href(Href, ajax) :-
	format("'javascript:loadContent(\"~w\");'", [Href]), !.
format_href(Href, _Html) :-
	format("~w", [Href]).
	
format_href(Pattern, NumberOfJugglers, SwapList, BackURL) :-	
	pattern_to_string(Pattern, PatternStr),
	list_to_string(SwapList, SwapListStr),
	format(atom(Href), "./info.php?pattern=~s&persons=~w&swap=~s&back=~w", [PatternStr, NumberOfJugglers, SwapListStr, BackURL]),
	format_href(Href).
	
if_ajax(Call, Type) :-
	(Type = ajax ->
		call(Call);
		true
	).
	