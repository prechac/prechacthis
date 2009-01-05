:- module(http_main_page,
	[
		main_page/1,
		mainPage_siteswap/6
	]
).


:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).

:- use_module(helpers).
:- use_module(siteswap_helpers).
:- use_module(siteswap_main_page).
:- use_module(siteswap_preprocessing).

:- use_module(http_helpers).
:- use_module(http_server).
:- use_module(http_common).
:- use_module(http_info_page).



main_page(Request) :-
	get_cookie(main_mode, Request, CookieMode, 'simple'),
	get_cookie(main_persons, Request, CookiePersons, 2),	
	get_cookie(main_max, Request, CookieMax, 4),
	get_cookie(main_passesmin, Request, CookiePassesMin, 1),
	get_cookie(main_passesmax, Request, CookiePassesMax, -1),
	get_cookie(main_results, Request, CookieResults, 42),
	
	a2Number(CookiePersons, CookiePersonsInt),
	a2Number(CookieMax, CookieMaxInt),
	a2Number(CookiePassesMin, CookiePassesMinInt),
	a2Number(CookiePassesMax, CookiePassesMaxInt),
	a2Number(CookieResults, CookieResultsInt),
	
	http_parameters(
		Request,
		[
			mode(ReqMode, [default(CookieMode)]),
			persons(ReqPersons, [integer, default(CookiePersonsInt)]),
			objects(ReqObjects, [default('')]),
			period(ReqPeriod, [default('')]),
			max(ReqMax, [integer, default(CookieMaxInt)]),
			passesmin(ReqPassesMin, [integer, default(CookiePassesMinInt)]),
			passesmax(ReqPassesMax, [integer, default(CookiePassesMaxInt)]),
			contain(ReqContain, [default('')]),
			exclude(ReqExclude, [default('')]),
			clubdoes(ReqClubDoes, [default('')]),
			react(ReqReact, [default('')]),
			magic(ReqMagic, [integer, default(0)]),
			results(ReqResults, [integer, default(CookieResultsInt)]),
			infopage(GoToInfoPage, [default('true')])
		]
	),
	
	retractall(href_type(_)),
	asserta(href_type(html)),
	
	set_cookie(main_mode, ReqMode),
	set_cookie(main_persons, ReqPersons),
	set_cookie(main_max, ReqMax),
	set_cookie(main_passesmin, ReqPassesMin),
	set_cookie(main_passesmax, ReqPassesMax),
	set_cookie(main_results, ReqResults),
	
	find_siteswap_lists(
		SearchResults,
		ReqPersons,
		ReqObjects,
		ReqPeriod,
		ReqMax,
		ReqPassesMin,
		ReqPassesMax,
		ReqContain,
		ReqExclude,
		ReqClubDoes,
		ReqReact,
		ReqMagic,
		ReqResults,
		Request
	),
	
	mainPage_html_page(
		SearchResults,
		ReqPersons, 
		ReqObjects, 
		ReqPeriod, 
		ReqMax, 
		ReqPassesMin, 
		ReqPassesMax, 
		ReqContain, 
		ReqExclude, 
		ReqClubDoes, 
		ReqReact, 
		ReqMagic, 
		ReqResults,
		ReqMode, 
		Request,
		GoToInfoPage
	).
	

mainPage_html_page(
	SearchResults,
	ReqPersons, 
	_ReqObjects, 
	_ReqPeriod, 
	_ReqMax, 
	_ReqPassesMin, 
	_ReqPassesMax, 
	_ReqContain, 
	_ReqExclude, 
	_ReqClubDoes, 
	_ReqReact, 
	_ReqMagic, 
	_ReqResults,
	_ReqMode, 
	Request,
	true
) :-
	http_main_page_path(MainPagePath),
	memberchk(SiteswapLists, SearchResults, [name(lists)]),
	SiteswapLists = [SiteswapList], 
	memberchk(siteswaps(Siteswaps), SiteswapList),
	Siteswaps = [Pattern],
	memberchk(search(BackURLSearchList), Request),
	parse_url_search(BackURLSearch, BackURLSearchList),
	format(atom(BackURL), '.~w?~s&infopage=false', [MainPagePath, BackURLSearch]),
	infoPage_html_page(Pattern, ReqPersons, [], BackURL, Request), !.
	

mainPage_html_page(
	SearchResults,
	ReqPersons, 
	ReqObjects, 
	ReqPeriod, 
	ReqMax, 
	ReqPassesMin, 
	ReqPassesMax, 
	ReqContain, 
	ReqExclude, 
	ReqClubDoes, 
	ReqReact, 
	ReqMagic, 
	ReqResults,
	ReqMode, 
	Request,
	_DontGoToInfoPage
) :-
	html_set_options([
			dialect(html), 
			doctype('HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"'),
			content_type('text/html; charset=UTF-8')
	]),
	reply_html_page(
		[
			title('PrechacThis'),
			meta(['http-equiv'('Content-Type'), content('text/html;charset=utf-8')]),
			link([type('text/css'), rel('stylesheet'), href('./css/common.css')]),
			link([type('text/css'), rel('stylesheet'), href('./css/main_page.css')]),
			link([rel('shortcut icon'), href('./images/favicon.png')])
			%\ajax_script
		],
		[
			\html_debug(Request),
			\mainPage_all_lists_of_siteswaps(
				SearchResults, 
				Request, 
				ReqPersons
			),
			\mainPage_form(
				ReqPersons, 
				ReqObjects, 
				ReqPeriod, 
				ReqMax, 
				ReqPassesMin, 
				ReqPassesMax, 
				ReqContain, 
				ReqExclude, 
				ReqClubDoes, 
				ReqReact, 
				ReqMagic, 
				ReqResults,
				ReqMode
			)
		]
	).



mainPage_all_lists_of_siteswaps(SearchResults, Request, Persons) -->
	{	
		http_main_page_path(MainPagePath),
		memberchk(path(MainPagePath), Request),
		memberchk(SiteswapLists, SearchResults, [name(lists)]),
		memberchk(flag(Flag), SearchResults),
		memberchk(time(Time), SearchResults),
		memberchk(period_searched(PeriodsSearched), SearchResults),
		memberchk(objects_searched(ObjectsSearched), SearchResults)
	},
	html(
		table([align(center), cellpadding(0)],[
			tr([],[
				td([],[
					\mainPage_search_results_info(Persons, Flag, Time, PeriodsSearched, ObjectsSearched)
				])
			]),
			tr([],[
				td([],[
					\mainPage_walk_list_of_siteswapLists(SiteswapLists, Request, Persons, first)
				])
			])
		])
	).
mainPage_all_lists_of_siteswaps(_Siteswaps, _Request, _Persons) -->
	[].		
	
	
mainPage_search_results_info(Persons, Flag, Time, PeriodsSearched, ObjectsSearched) -->
	mainPage_search_results_info_persons(Persons),
	mainPage_search_results_info_rest(Flag, Time, PeriodsSearched, ObjectsSearched).
	
mainPage_search_results_info_persons(1) -->
	html([
		h2([],['one juggler'])
	]), !.
mainPage_search_results_info_persons(Persons) -->
	html([
		h2([],[Persons, ' jugglers'])
	]).


mainPage_search_results_info_rest(_Flag, _Time, PeriodsSearched, ObjectsSearched) -->
	{
		length(PeriodsSearched, 0);
		length(ObjectsSearched, 0)
	},
	html([h3([],['oops: nothing searched!'])]),!.
mainPage_search_results_info_rest(_Flag, _Time, PeriodsSearched, ObjectsSearched) -->
	{
		length(PeriodsSearched, 1),
		length(ObjectsSearched, 1)
	},
	[],!.
mainPage_search_results_info_rest(time, Time, PeriodsSearched, ObjectsSearched) -->
	{
		TimeRound is round(Time)
	},
	html([
		div([class(mainPage_info), align(center)],[
			table([align(center), cellpadding(0)],[
				tr([],[
					td([colspan(2)],[
						'stopped searching after ', 
						TimeRound,
						' seconds!'
					])
				]),
				tr([],[
					td([class(mainPage_info_lable)],[
						'periods searched:'
					]),
					td([class(mainPage_info_info)],[
						\html_list(PeriodsSearched, [list_left(''), list_right(''), list_seperator(', ')])
					])
				]),
				tr([],[
					td([class(mainPage_info_lable)],[
						'objects searched:'
					]),
					td([class(mainPage_info_info)],[
						\html_list(ObjectsSearched, [list_left(''), list_right(''), list_seperator(', ')])
					])
				])
			])
		])
	]).

mainPage_search_results_info_rest(_All, Time, PeriodsSearched, ObjectsSearched) -->
	html([
		div([class(mainPage_info), align(center)],[
			table([align(center), cellpadding(0)],[
				tr([],[
					td([class(mainPage_info_lable)],[
						'periods searched:'
					]),
					td([class(info)],[
						\html_list(PeriodsSearched, [list_left(''), list_right(''), list_seperator(', ')])
					])
				]),
				tr([],[
					td([class(mainPage_info_info)],[
						'objects searched:'
					]),
					td([class(info)],[
						\html_list(ObjectsSearched, [list_left(''), list_right(''), list_seperator(', ')])
					])
				]),
				tr([],[
					td([class(mainPage_info_lable)],[
						'time needed:'
					]),
					td([class(mainPage_info_info)],[
						Time,
						' sec.'
					])
				])
			])
		])
	]).


mainPage_walk_list_of_siteswapLists([], _Request, _Persons, first) --> 
	html([h2([],['nothing found!'])]), !.
mainPage_walk_list_of_siteswapLists([], _Request, _Persons, not_first) --> [], !.
mainPage_walk_list_of_siteswapLists([SiteswapList|Rest], Request, Persons, _) -->
	{
		http_main_page_path(MainPagePath),
		memberchk(path(MainPagePath), Request),!,
		memberchk(flag(Flag), SiteswapList),
		memberchk(time(Time), SiteswapList),
		memberchk(objects(Objects), SiteswapList),
		(Objects = 1 -> ObjectsText = ' object'; ObjectsText = ' objects'),
		memberchk(length(Length), SiteswapList),
		memberchk(siteswaps(Siteswaps), SiteswapList),
		length(Siteswaps, NumberOfResults),
		(Flag = some -> 
			HowMany = p([class(some)],['selection of patterns!']);
			(Flag = time ->
				(NumberOfResults is 1 ->	
					HowMany = p([class(some)],['time limit reached, one pattern found!']);
					HowMany = p([class(some)],['time limit reached, ', NumberOfResults, ' patterns found!'])
				);
				(NumberOfResults is 1 ->	
					HowMany = p([class(all)],['one pattern found!']);
					HowMany = p([class(all)],[NumberOfResults, ' patterns found!'])
				)	
			)	
		),
		memberchk(BackURLSearchList, Request, [name(search), default([])]),
		parse_url_search(BackURLSearch, BackURLSearchList),
		format(atom(BackURL), ".~w?~s", [MainPagePath, BackURLSearch])
	},
	html(
		div([class(inline)],[
			div([class(swaps)],[
				h3([],[
					Objects, 
					ObjectsText,
					', period ', 
					Length
				]),
				HowMany,
				p([class(time)],['(', Time, ' seconds)']),
				\mainPage_list_of_siteswaps(Siteswaps, Persons, BackURL)
			])
		])
	),
	mainPage_walk_list_of_siteswapLists(Rest, Request, Persons, not_first).
	

mainPage_list_of_siteswaps([], _Persons, _BackURL) -->
	[],!.
mainPage_list_of_siteswaps([Siteswap|Siteswaps], Persons, BackURL) -->	
	mainPage_siteswap_link(Siteswap, Persons, BackURL),
	mainPage_list_of_siteswaps(Siteswaps, Persons, BackURL).

mainPage_siteswap_link(Throws, Persons, BackURL) -->
	{
		length(Throws, Length),
		float_to_shortpass(Throws, ThrowsShort),
		magicPositions(Throws, Persons, MagicPositions)
	},	
	html([
		p([],[
			\html_href(ThrowsShort, Persons, [], BackURL, [], \mainPage_siteswap(ThrowsShort, Length, Persons, MagicPositions))
		])
	]).
    %convertP(Throws, ThrowsP, Length, Persons),
	%convertMagic(ThrowsP, MagicPositions, ThrowsPM),
	%convertMultiplex(ThrowsPM,ThrowsPMM),
    %writeSwap(ThrowsPMM, Throws, Persons, BackURL).

mainPage_siteswap([], _Length, _Persons, _MagicPositions) --> [],!.
mainPage_siteswap([Throw], Length, Persons, MagicPositions) -->
	{
		Position is Length - 1, !
	},
	html_throw(Throw, [hideIndex(Persons), colorThrow(Length), magic(Position, MagicPositions)]).
mainPage_siteswap([Throw|RestThrows], Length, Persons, MagicPositions) -->
	{
		length(RestThrows, RestLength),
		Position is Length - RestLength - 1
	},
	html_throw(Throw, [hideIndex(Persons), colorThrow(Length), magic(Position, MagicPositions)]),
	html(&(nbsp)),
	mainPage_siteswap(RestThrows, Length, Persons, MagicPositions).



	
mainPage_form(Persons, Objects, Period, Max, PassesMin, PassesMax, Contain, Exclude, ClubDoes, React, Magic, Results, Mode) -->
	{
		http_main_page_path(MainPagePath)
	},
	html(
		form([action(MainPagePath), method(get)],[
			table([class(form_table), align(center), cellpadding(0)],[
				\mainPage_form_jugglers(Persons, Mode),
				\mainPage_form_objects(Objects, Mode),
				\mainPage_form_period(Period, Mode),
				\mainPage_form_max(Max, Mode),
				\mainPage_form_passes(PassesMin, PassesMax, Mode),
				\mainPage_form_contain(Contain, Mode),
				\mainPage_form_exclude(Exclude, Mode),
				\mainPage_form_clubdoes(ClubDoes, Mode),
				\mainPage_form_react(React, Mode),
				\mainPage_form_magic(Magic, Mode),
				\mainPage_form_results(Results, Mode),
				\mainPage_form_submit(Mode)
			])
		])
	).	
				
	
mainPage_form_jugglers(Persons, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'jugglers:'
			]),
			td([class(input)],[
				select([name(persons), size(1)],[
					\html_numbered_options(1, 10, Persons)
				])
			])	
		])
	]).

mainPage_form_objects(Objects, simple) -->	
	{
		a2Number(Objects, ObjectsInt, [default(4)])
	},
	html([
		tr([],[
			td([class(lable)],[
				'objects:'
			]),
			td([class(input)],[
				select([name(objects), size(1)],[
					\html_numbered_options(1, 20, ObjectsInt)
				])
			])
		])
	]), !.
mainPage_form_objects(Objects, _Mode) -->	
	html([
		tr([],[
			td([class(lable)],[
				'objects:'
			]),
			td([class(input)],[
				input([type(text), name(objects), value(Objects)])
			])
		])
	]).

mainPage_form_period(Period, simple) -->
	{
		a2Number(Period, PeriodInt, [default(4)])
	},
	html([
		tr([],[
			td([class(lable)],[
				'period:'
			]),
			td([class(input)],[
				select([name(period), size(1)],[
					\html_numbered_options(1, 10, PeriodInt)
				])
			])
		])
	]), !.
mainPage_form_period(Period, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'period:'
			]),
			td([class(input)],[
				input([type(text), name(period), value(Period)])

			])
		])
	]).
	
mainPage_form_max(Max, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'max height:'
			]),
			td([class(input)],[
				select([name(max), size(1)],[
					\html_numbered_options(1, 10, Max)
				])
			])
		])
 	]).

mainPage_form_passes(PassesMin, PassesMax, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'passes:'
			]),
			td([class(input)],[
				table([align(left), cellpadding(0)],[
					tr([],[
						td([class(lable)],[
							'min:'
						]),
						td([class(input)],[
							select([name(passesmin), size(1)],[
								\html_numbered_options(0, 9, PassesMin)
							])
						])
					]),
					tr([],[
						td([class(lable)],[
							'max:'
						]),
						td([class(input)],[
							select([name(passesmax), size(1)],[
								\html_numbered_options(0, 9, PassesMax),
								\html_option('-1', PassesMax, &(nbsp))
							])
						])
					])
				])
			])
		])
	]).

	
mainPage_form_contain(Contain, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'contain:'
			]),
			td([class(input)],[
				input([type(text), name(contain), value(Contain)])
			])
		])
	]).


mainPage_form_exclude(_Exclude, simple) --> [], !.
mainPage_form_exclude(Exclude, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'exclude:'
			]),
			td([class(input)],[
				input([type(text), name(exclude), value(Exclude)])
			])
		])
	]).

mainPage_form_clubdoes(_ClubDoes, simple) --> [], !.
mainPage_form_clubdoes(ClubDoes, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'club does:'
			]),
			td([class(input)],[
				input([type(text), name(clubdoes), value(ClubDoes)])
			])
		])
	]).

mainPage_form_react(_React, simple) --> [], !.
mainPage_form_react(React, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'cause:'
			]),
			td([class(input)],[
				input([type(text), name(react), value(React)])
			])
		])
	]).

mainPage_form_magic(_Magic, simple) --> [], !.
mainPage_form_magic(Magic, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'contain magic:'
			]),
			td([class(input)],[
				\html_checkbox('1', magic, Magic)
			])
		])
	]).

mainPage_form_results(_Results, simple) --> 
	html(
		input([type(hidden), name(results), value(42)])
	), !.
mainPage_form_results(Results, _Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				'max results:'
			]),
			td([class(input)],[
				input([type(text), name(results), value(Results)])
			])
		])
	]).
	
	
mainPage_form_submit(simple) -->
	html([
		tr([],[
			td([class(lable)],[
				a([class(small), href('./?mode=advanced')],['advanced mode'])
			]),
			td([class(input)],[
				input([type(hidden), name(mode), value(simple)]),
				input([type(submit), value('Generate')])
			])
		])
	]),!.	
mainPage_form_submit(_Mode) -->
	html([
		tr([],[
			td([class(lable)],[
				a([class(small), href('./?mode=simple')],['simple mode'])
			]),
			td([class(input)],[
				input([type(hidden), name(mode), value(advanced)]),
				input([type(submit), value('Generate')])
			])
		])
	]).
