:- module(http_helpers,
	[
		html_href/5,
		html_href/6,
		html_numbered_option/4,
		html_numbered_options/5,
		html_option/5,
		html_checkbox/5,
		css/3,
		js_script/3,
		ajax_script/2,
		www_form_encode_all/2,
		set_cookie/2,
		get_cookie/3,
		get_cookie/4
	]
).

:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).

:- use_module(helpers).
:- use_module(http_info_page).
:- use_module(http_main_page).

:- dynamic
	href_type/1.

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
	

html_option(Value, Selected, Text) -->
	{
		nonvar(Selected),
		a2Atom(Value, ValueA),
		a2Atom(Selected, SelectedA),
		ValueA = SelectedA, !
	},
	html(option([value(Value), selected(selected)],[Text])).
html_option(Value, _Selected, Text) -->
	html(option([value(Value)],[Text])).


html_checkbox(Value, Name, Checked) -->
	{
		nonvar(Checked),
		a2Atom(Value, ValueA),
		a2Atom(Checked, CheckedA),
		ValueA = CheckedA, !
	},
	html(input([type(checkbox), name(Name), value(Value), checked(checked)])).
html_checkbox(Value, Name, _Checked) -->
	html(input([type(checkbox), name(Name), value(Value)])).


%html_wrap_in_spans([], Content) -->
%	html(Content), !.
%html_wrap_in_spans([Attributes|ListOfAttributes], Content) -->
%	html(
%		span([Attributes],[\html_wrap_in_spans(ListOfAttributes, Content)])
%	).



css(URL) -->
	html_post(css,
		link([ 
			type('text/css'),
			rel('stylesheet'),
			href(URL)
		])
	).


js_script(URL) -->
	html_post(head, 
		script([ 
			src(URL),
			type('text/javascript')
		], [])
	).

ajax_script -->
	{
		href_type(ajax), !
	},
	js_script('./js/ajax.js').
ajax_script -->
	[].




www_form_encode_all(Decoded, Encoded) :-
	a2Atom(Decoded, DecodedAtom),
	www_form_encode(DecodedAtom, Encoded).


	
	
set_cookie(Name, Value) :-
	format('Set-Cookie: ~w=~w; Max-Age=31536000; path=/~n', [Name, Value]).


get_cookie(Name, Request, Cookie) :-
	get_cookie(Name, Request, Cookie, _Var), !.
get_cookie(Name, Request, Cookie, _Default) :-
	memberchk(cookie(Cookies), Request),
	memberchk(Name=Cookie, Cookies), !.
get_cookie(_Name, _Request, Default, Default) :- !.
	

request2url(Request, URL) :-
    %memberchk(search(Search), Request),
    %memberchk(path(Path), Request),
    parse_url(URL, Request).

