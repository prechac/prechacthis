
main_page(Request) :-
	http_main_page_path(MainPagePath),
	html_set_options([
			dialect(html), 
			doctype('HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"'),
			content_type('text/html; charset=UTF-8')
	]),
	http_parameters(
		Request,
		[ 
			persons(ReqPersons, [integer, default(2)]),
			objects(ReqObjects, [default('')]),
			period(ReqPeriod, [default('')]),
			max(ReqMax, [integer, default(4)]),
			passesmin(ReqPassesMin, [integer, default(1)]),
			passesmax(ReqPassesMax, [integer, default(-1)]),
			contain(ReqContain, [default('')]),
			exclude(ReqExclude, [default('')]),
			clubdoes(ReqClubDoes, [default('')]),
			react(ReqReact, [default('')]),
			magic(ReqMagic, [integer, default(0)]),
			results(ReqResults, [integer, default(42)])
			%hreftype(ReqHrefType, [default('html')])
		]
	),
	retractall(href_type(_)),
	%asserta(href_type(ReqHrefType)),
	(a2Number(ReqPassesMax, -1) -> ReqPassesMaxVar = _Var; ReqPassesMaxVar = ReqPassesMax),
	(
		(
			memberchk(path(MainPagePath), Request),
			get_time(Start),
			find_siteswaps(
				Siteswaps,
				Flag,
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
			),
			get_time(End),
			Time is End - Start
		);
		true
	),
	reply_html_page(
		[
			title('PrechacThis'),
			meta(['http-equiv'('Content-Type'), content('text/html;charset=utf-8')]),
			link([type('text/css'), rel('stylesheet'), href('./css/prechacthis.css')]),
			link([rel('shortcut icon'), href('./images/favicon.png')])
			%\ajax_script
		],
		[
			\mainPage_all_lists_of_siteswaps(
				Siteswaps, 
				Flag, 
				Request, 
				ReqPersons, 
				ReqObjects, 
				ReqPeriod, 
				Time
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
	).
	
	
	
	find_siteswaps(Siteswaps, Flag, Persons, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic, Results) :-
		findAtMostNUnique(Throws, 
		   siteswap(Throws, Persons, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic),
		   Results, 
		   Bag,
		   Flag
		),
		!,
		sortListOfSiteswaps(Bag, Siteswaps).
	
	
	
	mainPage_all_lists_of_siteswaps(Siteswaps, Flag, Request, Persons, Objects, Period, Time) -->
		{
			http_main_page_path(MainPagePath),
			memberchk(path(MainPagePath), Request),!,
			length(Siteswaps, NumberOfResults),
			(Flag = some -> 
				HowMany = p([class(some)],['Just a selection of patterns is shown!']);
				(NumberOfResults is 1 ->	
					HowMany = p([class(all)],['The only possible pattern has been found!']);
					HowMany = p([class(all)],['All ', NumberOfResults, ' patterns have been found!'])
				)		
			),
			memberchk(search(BackURLSearchList), Request),
			parse_url_search(BackURLSearch, BackURLSearchList),
			format(atom(BackURL), ".~w?~s", [MainPagePath, BackURLSearch])
		},
		html(
			table([align(center), cellpadding(0)],[
				tr([],[
					td([],[
						div([class(inline)],[
							div([class(swaps)],[
								h2([],[
									Persons,
									' Jugglers'
								]),
								h3([],[
									Objects, 
									' objects, period ', 
									Period
								]),
								HowMany,
								p([class(time)],['(', Time, ' seconds)']),
								\mainPage_list_of_siteswaps(Siteswaps, Persons, BackURL)
							])
						])
					])
				])
			])
		).
	mainPage_all_lists_of_siteswaps(_Siteswaps, _Flag, _Request, _Persons, _Objects, _Period, _Time) -->
		[].
		
		
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

		
mainPage_form_tr(Lable, Input) -->
	html(
		tr([],[
			td([class(lable)],[
				Lable
			]),
			td([class(input)],[
				Input
			])
		])
	).