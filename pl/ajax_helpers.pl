

html_href(Href, Attributes, Content) -->
	{
		href_type(ajax), !
	},
	html_href(Href, Attributes, Content, ajax).
html_href(Href, Attributes, Content) -->
	html_href(Href, Attributes, Content, html).

html_href(Href, Attributes, Content, ajax) -->
	{
		concat_atom(['javascript:loadContent("', Href, '");'], '', JSHref)
	},
	html_href(JSHref, Attributes, Content, html).
html_href(Href, Attributes, Content, html) -->
	html(a([href(Href)|Attributes], Content)).


html_href(Pattern, Persons, SwapList, BackURL, Attributes, Content) -->
	{
		float_to_shortpass(Pattern, PatternShort),
		www_form_encode_all(PatternShort, PatternEnc),	
		www_form_encode_all(Persons, PersonsEnc),
		www_form_encode_all(SwapList, SwapListEnc),
		www_form_encode_all(BackURL, BackURLEnc),
		parse_url_search(Search, [pattern(PatternEnc), persons(PersonsEnc), swap(SwapListEnc), back(BackURLEnc)]),
		http_info_page_path(Path),
		format(atom(Href), ".~w?~s", [Path, Search])
	},
	html_href(Href, Attributes, Content).

www_form_encode_all(Decoded, Encoded) :-
	a2Atom(Decoded, DecodedAtom),
	www_form_encode(DecodedAtom, Encoded).


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
	