

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
			persons(Persons, [default('2')]),
			objects(Objects, [default('')]),
			period(Period, [default('')]),
			max(Max, [default('4')]),
			passesmin(PassesMin, [default('1')]),
			passesmax(PassesMax, [default('-1')]),
			contain(Contain, [default('')]),
			exclude(Exclude, [default('')]),
			clubdoes(ClubDoes, [default('')]),
			react(React, [default('')]),
			magic(Magic, [default('0')]),
			results(Results, [default('42')]),
			hreftype(HrefType, [default('html')])
		]
	),
	retractall(href_type(_)),
	asserta(href_type(HrefType)),
	preprocess_number(Persons, PersonsN),
	%atom_number(Objects, ObjectsN),
	%atom_number(Period, PeriodN),
	(
		(
			memberchk(path(MainPagePath), Request),
			get_time(Start),
			find_siteswaps(Siteswaps, Flag, Persons, Objects, Period, Max, PassesMin, PassesMax, Contain, Exclude, ClubDoes, React, Magic, Results),
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
			
		],
		[
			\mainPage_all_lists_of_siteswaps(Siteswaps, Flag, Request, PersonsN, Objects, Period, Time),
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
		]
	).
	
	find_siteswaps(Siteswaps, Flag, PersonsPre, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic, MaxNumberOfResultsPre) :-
		preprocess_number(MaxNumberOfResultsPre, MaxNumberOfResults),
		preprocess_number(PersonsPre, Persons),
		findAtMostNUnique(Throws, 
		   siteswap(Throws, Persons, Objects, Length, Max, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic),
		   MaxNumberOfResults, 
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
			memberchk(search(BackURLSearch), Request)
			%concat_atom([_,URIDec], MainPagePath, BackURLDec),
			%www_form_encode(URIDec, BackURLEnc)
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
								\mainPage_list_of_siteswaps(Siteswaps, Persons, BackURLSearch)
							])
						])
					])
				])
			])
		).
	mainPage_all_lists_of_siteswaps(_Siteswaps, _Flag, _Request, _Persons, _Objects, _Period, _Time) -->
		[].
	
		
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