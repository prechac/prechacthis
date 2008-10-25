

html_href(Href, Attributes, Content) -->
	{
		href_type(ajax), !
	},
	html_href(Href, Attributes, Content, ajax).
html_href(Href, Attributes, Content) -->
	html_href(Href, Attributes, Content, html).

html_href(Href, Attributes, Content, ajax) -->
	{
		concat_atom(['javascript:loadContent(', Href, ');'], '', JSHref)
	},
	html_href(JSHref, Attributes, Content, html).
html_href(Href, Attributes, Content, html) -->
	html(a([href(Href)|Attributes], Content)).


html_href(Pattern, NumberOfJugglers, SwapList, BackURL, Attributes, Content) -->
	{
	%	www_form_encode(Pattern, PatternEnc),
		a2Atom(NumberOfJugglers, NumberOfJugglersA),
		www_form_encode_all(NumberOfJugglersA, NumberOfJugglersEnc)
	%	www_form_encode(SwapList, SwapListEnc),
	%	www_form_encode(BackURL, BackURLEnc)
	%	parse_url_search(Search, [pattern(PatternEnc), persons(NumberOfJugglersEnc), swap(SwapListEnc), back(BackURLEnc)]),
	%	http_info_page_path(Path),
	%	concat_atom(['.', Path, '?', Search], Href)
	},
	html_href(NumberOfJugglersEnc, Attributes, Content).

www_form_encode_all(Decoded, Encoded) :-
	atom(Decoded), !,
	www_form_encode(Decoded, Encoded).


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
	a2Atom(Pattern, PatternStr),
	a2Atom(SwapList, SwapListStr),
	format(atom(Href), "./info.php?pattern=~w&amp;persons=~w&amp;swap=~w&amp;back=~w", [PatternStr, NumberOfJugglers, SwapListStr, BackURL]),
	format_href(Href).
	
	
	
	
	
	
if_ajax(Call, Type) :-
	(Type = ajax ->
		call(Call);
		true
	).
	