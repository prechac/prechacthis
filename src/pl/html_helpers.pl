	
html_numbered_option([], _Selected) --> [].
html_numbered_option([Value|List], Selected) -->
	html_numbered_option(Value, Selected),
	html_numbered_option(List, Selected).
html_numbered_option(Value, Selected) -->
	{
		number(Value)
	},
	html_option(Value, Selected, Value).

html_numbered_options(Min, Max, Selected) -->
	{
		numlist(Min, Max, List)
	},
	html_numbered_option(List, Selected).
	

html_option(Term, Selected, Text) -->
	{
		number(Term), !,
		term_to_atom(Term, Atom)
	},
	html_option(Atom, Selected, Text).
html_option(Value, Selected, Text) -->
	{
		nonvar(Selected),
		Value = Selected, !
	},
	html(option([value(Value), selected(selected)],[Text])).
html_option(Value, _Selected, Text) -->
	html(option([value(Value)],[Text])).


html_checkbox(Value, Name, Checked) -->
	{
		nonvar(Checked),
		Value = Checked, !
	},
	html(input([type(checkbox), name(Name), value(Value), checked(checked)])).
html_checkbox(Value, Name, _Checked) -->
	html(input([type(checkbox), name(Name), value(Value)])).


css(URL) -->
	html_post(css,
		link([ 
			type('text/css'),
			rel('stylesheet'),
			href(URL)
		])
	).


js_script(URL) -->
	html_post(head, script([ 
		src(URL),
		type('text/javascript')
		], [])
	).



