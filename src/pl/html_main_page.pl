


main_page(Request) :-
	http_parameters(
		Request,
		[ 
			persons(Persons, [default('2')]),
			objects(Objects, [default('')]),
			period(Period, [default('')]),
			max(Max, [default('4')]),
			passesmin(Passesmin, [default('1')]),
			passesmax(Passesmax, [default('-1')]),
			contain(Contain, [default('')]),
			exclude(Exclude, [default('')]),
			clubdoes(Clubdoes, [default('')]),
			react(React, [default('')]),
			magic(Magic, [default('0')]),
			results(Results, [default('')])
		]
	),
	reply_html_page(
		[
			title('PrechacThis'),
			meta(['http-equiv'('Content-Type'), content('text/html;charset=utf-8')]),
			link([type('text/css'), rel('stylesheet'), href('./css/prechacthis.css')]),
			link([rel('shortcut icon'), href('./images/favicon.png')])
			
		],
		[
			table([align(center), cellpadding(0)],[
				tr([],[
					td([],[
						form([action('./list'), method(get)],[
							table([align(center), cellpadding(10)],[
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
										span([],['min:', &(nbsp)]),
										select([name(passesmin), size(1)],[
											\html_numbered_options(0, 9, Passesmin)
										])
									])
								]),
								tr([],[
									td([class(lable)],[
										&(nbsp)
									]),
									td([class(input)],[
										span([],['max:', &(nbsp)]),
										select([name(passesmax), size(1)],[
											\html_numbered_options(0, 9, Passesmax),
											\html_option('-1', Passesmax, &(nbsp))
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
										input([type(text), name(clubdoes), value(Clubdoes)])
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
					])
				])
			])
		]
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