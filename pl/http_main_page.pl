
main_page(Request) :-
	http_main_page_path(MainPagePath),
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
			debug(Debug, [default('off')]),
			infopage(GoToInfoPage, [default('true')])
		]
	),
	
	retractall(href_type(_)),
	asserta(href_type(html)),
	
	set_cookie(main_persons, ReqPersons),
	set_cookie(main_max, ReqMax),
	set_cookie(main_passesmin, ReqPassesMin),
	set_cookie(main_passesmax, ReqPassesMax),
	set_cookie(main_results, ReqResults),
	
	(Debug = on -> DebugHTML = pre([],[\[Request]]); DebugHTML = ''),
	
	(a2Number(ReqPassesMax, -1) -> ReqPassesMaxVar = _Var; ReqPassesMaxVar = ReqPassesMax),
	(
		(
			memberchk(path(MainPagePath), Request),
			find_siteswap_lists(
				SearchResults,
				ReqPersons,
				ReqObjects,
				ReqPeriod,
				ReqMax,
				ReqPassesMin,
				ReqPassesMaxVar,
				ReqContain,
				ReqExclude,
				ReqClubDoes,
				ReqReact,
				ReqMagic,
				ReqResults
			)
		);
		SearchResults = false
	),
	(
		(
			memberchk(SiteswapLists, SearchResults, [name(lists)]),
			GoToInfoPage = true,
			SiteswapLists = [SiteswapList], 
			memberchk(siteswaps(Siteswaps), SiteswapList),
			Siteswaps = [Pattern]
		) -> 
		(
			memberchk(search(BackURLSearchList), Request),
			parse_url_search(BackURLSearch, BackURLSearchList),
			format(atom(BackURL), '.~w?~s&infopage=false', [MainPagePath, BackURLSearch]),
			infoPage_html_page(Pattern, ReqPersons, [], BackURL, Request)
		) ;
		(
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
					DebugHTML,
					\mainPage_all_lists_of_siteswaps(
						SearchResults, 
						Request, 
						ReqPersons
					),
					\mainPage_form(
						MainPagePath,
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
						ReqResults
					)
				]
			)
		)	
	).
	
	

find_siteswap_lists(SearchResults, PersonsInt, ObjectsAtom, LengthAtom, MaxInt, PassesMinInt, PassesMaxInt, ContainAtom, DontContainAtom, ClubDoesAtom, ReactAtom, MagicInt, ResultsInt) :-
	name(ContainAtom, ContainList),
	name(DontContainAtom, DontContainList),
	name(ClubDoesAtom, ClubDoesList),
	name(ReactAtom, ReactList),
	
	forall(recorded(period_searched, _, R), erase(R)),
	forall(recorded(objects_searched, _, R), erase(R)),
	get_time(Start),
	findall_restricted(
		Siteswaps,
		(
			preprocess_number(LengthAtom, LengthInt, [to_come(_PeriodsToCome), default('1-5')]),
			recordz(period_searched, LengthInt),
			preprocess_number(ObjectsAtom, ObjectsInt, [to_come(_ObjectsToCome), default('>0'), stop_if(test_constraint_not_fillable)]),
			recordz(objects_searched, ObjectsInt),
			find_siteswaps(Siteswaps, PersonsInt, ObjectsInt, LengthInt, MaxInt, PassesMinInt, PassesMaxInt, ContainList, DontContainList, ClubDoesList, ReactList, MagicInt, ResultsInt)
		),
		SiteswapLists,
		[time(20), flag(Flag)]
	),
	get_time(End),
	Time is End - Start,
	findall_restricted(Period, (recorded(period_searched, Period, R), erase(R)), PeriodsSearched, [unique]),
	findall_restricted(Objects, (recorded(objects_searched, Objects, R), erase(R)), ObjectsSearched, [unique]),
	SearchResults = [
		flag(Flag),
		time(Time),
		period_searched(PeriodsSearched),
		objects_searched(ObjectsSearched),
		lists(SiteswapLists)
	].


find_siteswaps(SiteswapList, Persons, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic, Results) :-
	get_time(Start),
	findall_restricted(
		Throws, 
		siteswap(Throws, Persons, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic),
		Bag,
		[unique, results(Results), flag(Flag)]
	),
	length(Bag, NumberOfSiteswaps),
	NumberOfSiteswaps > 0, !,
	sortListOfSiteswaps(Bag, Siteswaps),
	get_time(End),
	Time is End - Start,
	SiteswapList = [
		flag(Flag),
		time(Time),
		objects(Objects), 
		length(Length),
		siteswaps(Siteswaps)
	].



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
					\mainPage_walk_list_of_siteswapLists(SiteswapLists, Request, Persons)
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
		h2([],[Persons, ' juggers'])
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
	html([
		div([class(mainPage_info), align(center)],[
			table([align(center), cellpadding(0)],[
				tr([],[
					td([colspan(2)],[
						'stopped searching after ', 
						Time,
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


mainPage_walk_list_of_siteswapLists([], _Request, _Persons) --> [], !.
mainPage_walk_list_of_siteswapLists([SiteswapList|Rest], Request, Persons) -->
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
			HowMany = p([class(some)],['Just a selection of patterns is shown!']);
			(Flag = time ->
				HowMany = p([class(some)],['Time limit reached, that\'s what has been found!']);
				(NumberOfResults is 1 ->	
					HowMany = p([class(all)],['The only possible pattern has been found!']);
					HowMany = p([class(all)],['All ', NumberOfResults, ' patterns have been found!'])
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
	mainPage_walk_list_of_siteswapLists(Rest, Request, Persons).
	

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



	
mainPage_form(MainPagePath, Persons, Objects, Period, Max, PassesMin, PassesMax, Contain, Exclude, ClubDoes, React, Magic, Results) -->
	html(
		form([action(MainPagePath), method(get)],[
			table([class(form_table), align(center), cellpadding(0)],[
				tr([],[
					td([class(lable)],[
						'Jugglers:'
					]),
					td([class(input)],[
						select([name(persons), size(1)],[
							\html_numbered_options(1, 10, Persons)
						])
					])	
				]),	
				tr([],[
					td([class(lable)],[
						'Objects:'
					]),
					td([class(input)],[
						input([type(text), name(objects), value(Objects)])
					])
				]),
				tr([],[
					td([class(lable)],[
						'Period:'
					]),
					td([class(input)],[
						input([type(text), name(period), value(Period)])
		
					])
				]),
				tr([],[
					td([class(lable)],[
						'Max height:'
					]),
					td([class(input)],[
						select([name(max), size(1)],[
							\html_numbered_options(1, 10, Max)
						])
					])
				]),
				tr([],[
					td([class(lable)],[
						'Passes:'
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
				]),
				tr([],[
					td([class(lable)],[
						'Contain:'
					]),
					td([class(input)],[
						input([type(text), name(contain), value(Contain)])
					])
				]),
				tr([],[
					td([class(lable)],[
						'Exclude:'
					]),
					td([class(input)],[
						input([type(text), name(exclude), value(Exclude)])
					])
				]),
				tr([],[
					td([class(lable)],[
						'Club does:'
					]),
					td([class(input)],[
						input([type(text), name(clubdoes), value(ClubDoes)])
					])
				]),
				tr([],[
					td([class(lable)],[
						'React:'
					]),
					td([class(input)],[
						input([type(text), name(react), value(React)])
					])
				]),
				tr([],[
					td([class(lable)],[
						'Contain magic:'
					]),
					td([class(input)],[
						\html_checkbox('1', magic, Magic)
					])
				]),
				tr([],[
					td([class(lable)],[
						'Max results:'
					]),
					td([class(input)],[
						input([type(text), name(results), value(Results)])
					])
				]),
				tr([],[
					td([class(lable)],[
					]),
					td([class(input)],[
						input([type(submit), value('Generate')])
					])
				])
			])
		])
	).	

		