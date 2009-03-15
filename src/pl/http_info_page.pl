:- module(http_info_page, 
	[
		info_page/1,
		infoPage_html_page/5,
		arrowRightLeft/3,
		arrowUpDown/3,
		html_href/8,
		handShown/4
	]
).


:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/http_header')).

:- use_module(helpers).
:- use_module(siteswap_helpers).
:- use_module(siteswap_engine).
:- use_module(siteswap_info_page).
:- use_module(siteswap_multiplex).
:- use_module(siteswap_constraints).
:- use_module(siteswap_preprocessing).
:- use_module(http_server).

:- use_module(http_helpers).
:- use_module(http_common).
:- use_module(http_joepass).


info_page(Request) :-
	%http_info_page_path(InfoPagePath),
	http_parameters(
		Request,
		[ 
			pattern( ReqPattern,  [optional(false)]         ),
			persons( ReqPersons,  [optional(false), integer]),
			swap(    ReqSwap,     [default('[]')]           ),
			newswap( ReqNewSwap,  [default('[]')]           ),
			back(    ReqBack,     [default('')]             ),
			hreftype(ReqHrefType, [default('html')]         ),
			ajax(    Ajax,        [default('off')]          )
		]
	),
	retractall(href_type(_)),
	asserta(href_type(ReqHrefType)),
	
	atom2Pattern(ReqPattern, Pattern),
	atom2SwapList(ReqSwap, OldSwapList),
	atom2SwapList(ReqNewSwap, NewSwapList),
	Persons = ReqPersons,
	BackURL = ReqBack,
		
	applyNewSwaps(OldSwapList, NewSwapList, SwapList),
	
	(Ajax = 'on' ->
		infoPage_html_page_just_content(Pattern, Persons, SwapList, BackURL, Request);
		infoPage_html_page(Pattern, Persons, SwapList, BackURL, Request)
	).
	
infoPage_html_page_just_content(Pattern, Persons, SwapList, BackURL, Request) :-
	phrase(infoPage_info(Pattern, Persons, SwapList, BackURL, Request), HTML),
	print_html(HTML).
	
infoPage_html_page(Pattern, Persons, SwapList, BackURL, Request) :-
	html_set_options([
			dialect(html), 
			doctype('HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"'),
            content_type('text/html; charset=UTF-8')
	]),
	reply_html_page(
		[
			title('PrechacThis - Pattern Information'),
            meta(['http-equiv'('Content-Type'), content('text/html;charset=utf-8')]),
			link([type('text/css'), rel('stylesheet'), href('./css/common.css')]),
			link([type('text/css'), rel('stylesheet'), href('./css/info_page.css')]),
			link([rel('shortcut icon'), href('./images/favicon.png')]),
			\js_script('./js/ajax.js'),
			\js_script('./js/prototype/prototype.js'),
			\js_script('./js/scriptaculous/scriptaculous.js')
		],
		[
			\html_debug(Request),
			\infoPage_head(BackURL),
			table([align(center), cellpadding(0)],[
				tr([],[
					td([],[
						div([id(content), align(center)],[
							\infoPage_info(Pattern, Persons, SwapList, BackURL, Request)
						])
					])
				])
			]),					
			\infoPage_foot(BackURL),
			script([src('./js/swap.js'), type('text/javascript')], [])
		]
	).
	

infoPage_info(PatternWithShortPasses, NumberOfJugglers, SwapList, BackURL, Request) -->
	{
		init_html_throw_id,
		length(PatternWithShortPasses, Period),
		maxHeight(PatternWithShortPasses, ShortMaxHeight),
		MaxHeight is ceiling(ShortMaxHeight),
		convertShortPasses(PatternWithShortPasses, Period, NumberOfJugglers, MaxHeight, Pattern),
		all_points_in_time(PointsInTime, NumberOfJugglers, Period),
		what_happens(PointsInTime, Pattern, NumberOfJugglers, ActionList),
		averageNumberOfClubs(Pattern, AverageNumberOfClubs),
		NumberOfClubs is AverageNumberOfClubs * NumberOfJugglers,
		orbits(Pattern, OrbitPattern),
		club_distribution(ActionList, OrbitPattern, NumberOfClubs, NumberOfJugglers, Period, ClubDistribution),
		JugglerMax is NumberOfJugglers - 1,
		numlist(0, JugglerMax, ListOfJugglers)
	},
	infoPage_pattern(Pattern, NumberOfJugglers, SwapList, BackURL),
	infoPage_pattern_info(Pattern, PointsInTime, ActionList, NumberOfJugglers, SwapList, BackURL),
	html([
		form([id(joepass_form), action('./joe.pass'), method(post)],[
			\infoPage_orbit_info(Pattern, NumberOfJugglers),
			\infoPage_juggler_info(ListOfJugglers, ActionList, SwapList, ClubDistribution, NumberOfJugglers, Period, Pattern, BackURL),
			\infoPage_joepass_link(Pattern, NumberOfJugglers, SwapList, Request)
		])
	]),
	infoPage_hidden_info(Pattern, NumberOfJugglers, SwapList, BackURL).
	

infoPage_head(BackURL) -->
	html(
		table([class(info_head_foot), align(center), cellpadding(0)],[
			tr([],[
				td([class(back)],[
					a([href(BackURL)],[
						'back to search results'
					])
				]),
				td([id(linkhere)],[
					&(nbsp)
				])
			])
		])
	).

infoPage_foot(BackURL) -->
	html(
		table([class(info_head_foot), align(center), cellpadding(0)],[
			tr([],[
				td([class(back)],[
					a([href(BackURL)],[
						'back to search results'
					])
				])
			])
		])
	).


%%% --- pattern --- %%%	


infoPage_pattern(Pattern, NumberOfJugglers, SwapList, BackURL) -->
html([
	table([class(info_bigSwap_table), align(center)],[
		\infoPage_PrechacThis_links(Pattern, up, NumberOfJugglers, SwapList, BackURL),
		\infoPage_bigSwap_and_rotations(Pattern, NumberOfJugglers, SwapList, BackURL),
		\infoPage_PrechacThis_links(Pattern, down, NumberOfJugglers, SwapList, BackURL)
	])
]).


infoPage_bigSwap_and_rotations(Pattern, NumberOfJugglers, SwapList, BackURL) -->
{
	rotate_left(Pattern, PatternRotatedLeft),
	rotate_right(Pattern, PatternRotatedRight)
},
html(
	tr([],[
		td([class(info_left_arrow)],[
			\html_href(PatternRotatedLeft, NumberOfJugglers, SwapList, BackURL, [title('rotate left')], \arrowRightLeft(left))
		]),
		\infoPage_big_swap(Pattern, NumberOfJugglers),
		td([class(info_right_arrow)],[
			\html_href(PatternRotatedRight, NumberOfJugglers, SwapList, BackURL, [title('rotate right')], \arrowRightLeft(right))
		])
	])
).

	
infoPage_big_swap(Pattern, NumberOfJugglers) -->
	{
		length(Pattern, Length),
		magicPositions(Pattern, NumberOfJugglers, MagicPositions)
	},
	infoPage_big_throw(Pattern, Length, NumberOfJugglers, MagicPositions).
	

infoPage_big_throw([], _, _, _) --> [], !.
infoPage_big_throw([Throw|Rest], Length, NumberOfJugglers, MagicPositions) -->
	{
		length(Rest, RestLength),
		Position is Length - RestLength - 1
	},
	html([
		\html_throw(Throw, [td([class('bigSwap')]), id(throw), hideIndex(NumberOfJugglers), colorThrow(Length), magic(Position, MagicPositions)])
	]),
	infoPage_big_throw(Rest, Length, NumberOfJugglers, MagicPositions).
	
infoPage_PrechacThis_links(Pattern, UpDown, NumberOfJugglers, SwapList, BackURL) -->
	{
		posList(Pattern, PosList)
	},
	html([
		tr([],[
			td([],[
				&(nbsp)
			]),
			\infoPage_PrechacThis_link_calc(PosList, Pattern, UpDown, NumberOfJugglers, SwapList, BackURL),
			td([class(info_lable)],[
                \infoPage_add_sub_throw(Pattern, NumberOfJugglers, SwapList, BackURL, UpDown)
			])
		])
	]).
	
infoPage_PrechacThis_link_calc([], _, _, _, _, _) --> [].
infoPage_PrechacThis_link_calc([Pos|PosList], Pattern, UpDown, NumberOfJugglers, SwapList, BackURL) -->
	{
		prechacThis(Pattern, Pos, UpDown, NumberOfJugglers, NewPattern)
	},
	infoPage_PrechacThis_link(NewPattern, UpDown, NumberOfJugglers, SwapList, BackURL),
	infoPage_PrechacThis_link_calc(PosList, Pattern, UpDown, NumberOfJugglers, SwapList, BackURL).
	
infoPage_PrechacThis_link(false, _UpDown, _NumberOfJugglers, _SwapList, _BackURL) -->
	{!},
	html(td([class(prechacthis_link)],[&(nbsp)])).
infoPage_PrechacThis_link(Pattern, UpDown, NumberOfJugglers, SwapList, BackURL) -->	
	{
		atom_concat('PrechacThis ', UpDown, Title)
	},
	html(
		td([class(prechacthis_link)],[
			\html_href(Pattern, NumberOfJugglers, SwapList, BackURL, [title(Title)], \arrowUpDown(UpDown))
		])
	).


arrowUpDown(up) -->
	html(img([src('./images/up.png'), alt('up'), border(0)])).
arrowUpDown(down) -->
	html(img([src('./images/down.png'), alt('down'), border(0)])).
	

arrowRightLeft(right) -->
	html(img([src('./images/right_arrow.png'), alt('right'), border(0)])).
arrowRightLeft(left) -->
	html(img([src('./images/left_arrow.png'), alt('left'), border(0)])).
	

infoPage_add_sub_throw(Pattern, NumberOfJugglers, SwapList, BackURL, down) -->
	{
		amountOfPasses(Pattern, 0), 
        noMultiplex(Pattern),
        landingSites(Pattern, LandingSites, real),
        min_list(LandingSites, Min),
        length(Pattern, Length),
        Max is Min + Length - 1,
        numlist(Min, Max, Permutation),
        permutation(LandingSites, Permutation), !,
        originalNumberOfClubs(Pattern, Clubs),
        append(Pattern, [p(Clubs, 0, Clubs)], PatternPlus)
	},
	html([
        \html_href(PatternPlus, NumberOfJugglers, SwapList, BackURL, [class(small), title('add Throw')], 'add'),
        \infoPage_sub_throw(Pattern, Clubs, NumberOfJugglers, SwapList, BackURL)
    ]).
infoPage_add_sub_throw(_Pattern, _NumberOfJugglers, _SwapList, _BackURL, _UpDown) -->
    html([
        &(nbsp)
    ]).

infoPage_sub_throw(Pattern, Clubs, NumberOfJugglers, SwapList, BackURL) -->
    {
        append(PatternMinus, [p(Clubs, 0, Clubs)], Pattern)
    },
    html([
        &(nbsp), '|', &(nbsp),
        \html_href(PatternMinus, NumberOfJugglers, SwapList, BackURL, [class(small), title('subtract Throw')], 'sub')
    ]).
infoPage_sub_throw(_Pattern, _Clubs, _NumberOfJugglers, _SwapList, _BackURL) -->
	[].
    

%%% --- pattern info --- %%%

infoPage_pattern_info(Pattern, PointsInTime, ActionList, NumberOfJugglers, SwapList, BackURL) -->
	{
		length(Pattern, Period),
		JugglerMax is NumberOfJugglers - 1,
		numlist(0, JugglerMax, ListOfJugglers)		
	},
	html([
		table([class(info_pattern_table), align(center)],[
			\infoPage_jugglers_throws(ListOfJugglers, ActionList, PointsInTime, NumberOfJugglers, Period),
			\infoPage_add_sub_juggler(Pattern, NumberOfJugglers, SwapList, BackURL)
		])
	]).


infoPage_add_sub_juggler(Pattern, NumberOfJugglers, SwapList, BackURL) -->
	{
		amountOfPasses(Pattern, 0), !,
		NumberOfJugglersPlus is NumberOfJugglers + 1,
		NumberOfJugglersMinus is NumberOfJugglers - 1,
		length(Pattern, Period)
	},
	html([
		tr([],[
			td([class(info_lable)],[
				\html_href(Pattern, NumberOfJugglersPlus, SwapList, BackURL, [class(small), title('add Juggler')], 'add'),
				&(nbsp), '|', &(nbsp),
				\html_href(Pattern, NumberOfJugglersMinus, SwapList, BackURL, [class(small), title('subtract Juggler')], 'sub')
			]),
			td([colspan(Period)],[&(nbsp)])
		])
	]).
infoPage_add_sub_juggler(_Pattern, _NumberOfJugglers, _SwapList, _BackURL) -->
	[].

infoPage_jugglers_throws([], _, _, _, _) --> [], !.
infoPage_jugglers_throws([Juggler|ListOfJugglers], ActionList, PointsInTime, NumberOfJugglers, Period) -->
	{
		jugglerShown(Juggler, JugglerShown)
	},
	html([
		tr([],[
			td([class(info_lable_swap)],['juggler ', JugglerShown, ':']),
			\infoPage_jugglers_point_in_time(Juggler, PointsInTime, ActionList, NumberOfJugglers, Period)
		])
	]),
	infoPage_jugglers_throws(ListOfJugglers, ActionList, PointsInTime, NumberOfJugglers, Period).
	

infoPage_jugglers_point_in_time(_, [], _, _, _) --> [], !.
infoPage_jugglers_point_in_time(ThrowingJuggler, [Point|PointsInTime], ActionList, NumberOfJugglers, Period) -->
	{
		member(Action, ActionList),
		nth1(2, Action, ThrowingJuggler),
		nth1(1, Action, Point),!,
		nth1(4, Action, Throw)
	},
	html([
		td([class(info_throw)],[
			\html_throw(Throw, [hideIndex(NumberOfJugglers), colorThrow(Period), absoluteIndex(NumberOfJugglers, ThrowingJuggler)])
		])
	]),
	infoPage_jugglers_point_in_time(ThrowingJuggler, PointsInTime, ActionList, NumberOfJugglers, Period).
infoPage_jugglers_point_in_time(ThrowingJuggler, [_Point|PointsInTime], ActionList, NumberOfJugglers, Period) -->
	html(
		td([class(info_throw)],[
			&(nbsp)
		])
	),
	infoPage_jugglers_point_in_time(ThrowingJuggler, PointsInTime, ActionList, NumberOfJugglers, Period).




%%% --- orbit info --- %%%


infoPage_orbit_info(Pattern, NumberOfJugglers) -->
	{
		length(Pattern, Period),
		magicPositions(Pattern, NumberOfJugglers, MagicPositions),
		orbits(Pattern, OrbitPattern),
		flatten(OrbitPattern, OrbitsFlat),
		list_to_set(OrbitsFlat, OrbitsSet),
		sort(OrbitsSet, Orbits),
		Colspan is Period,
		averageNumberOfClubs(Pattern, AVClubs),
		Clubs is AVClubs * NumberOfJugglers
	},
	html([
		table([class(info_pattern_table), align(center)],[
			tr([],[
				td([class(info_title), colspan(Colspan)],[
					'orbits'
				]),
				td([class(info_right_info)],[
					Clubs, ' clubs'
				]),
				\infoPage_orbit_info_ColorHead(Pattern)
			]),
			\infoPage_this_orbit_info(OrbitPattern, Orbits, Pattern, NumberOfJugglers, Period, MagicPositions)
		])
	]).

infoPage_orbit_info_ColorHead(Pattern) -->
	{
		noMultiplex(Pattern)
	},
	html(	
		td([class(info_right_info)],[
			'orbit color'
		])
	).
infoPage_orbit_info_ColorHead(_) --> [], !.
	
	
infoPage_this_orbit_info(_OrbitPattern, [], _Pattern, _NumberOfJugglers, _Period, _MagicPositions) --> [], !.
infoPage_this_orbit_info(OrbitPattern, [Orbit|Orbits], Pattern, NumberOfJugglers, Period, MagicPositions) -->
	{
		clubsInOrbit(Pattern, OrbitPattern, Orbit, ClubsAV),
		Clubs is ClubsAV * NumberOfJugglers
	},
	html([
		tr([],[
			\infoPage_just_this_orbit(Pattern, OrbitPattern, Orbit, Clubs, NumberOfJugglers, Period, MagicPositions, Pattern)
		])
	]),
	infoPage_this_orbit_info(OrbitPattern, Orbits, Pattern, NumberOfJugglers, Period, MagicPositions).


infoPage_just_this_orbit([], _OrbitPattern, Orbit, Clubs, _NumberOfJugglers, _Period, _MagicPositions, Pattern) --> 
	infoPage_just_this_orbit_NumberOfClubs(Clubs),
	infoPage_just_this_orbit_OrbitColor(Clubs, Orbit, Pattern), !.
infoPage_just_this_orbit([Throw|Rest], [ThisOrbit|OrbitPattern], JustOrbit, Clubs, NumberOfJugglers, Period, MagicPositions, Pattern) -->
	{
		length(Rest, RestLength),
		Position is Period - RestLength - 1
	},
	html([
		td([class(info_throw)],[
			\html_throw(Throw, [orbit(JustOrbit, ThisOrbit), hideIndex(NumberOfJugglers), colorThrow(Period), magic(Position, MagicPositions)])
		])
	]), !,
	infoPage_just_this_orbit(Rest, OrbitPattern, JustOrbit, Clubs, NumberOfJugglers, Period, MagicPositions, Pattern).


infoPage_just_this_orbit_NumberOfClubs(Clubs) -->
	html([
		td([class(info_right_info)],[
			Clubs
		])
	]).
infoPage_just_this_orbit_OrbitColor(Clubs, Orbit, Pattern) -->
	{
		noMultiplex(Pattern)
	},
	html([
		td([class(info_right_info)],[
			\infoPage_orbit_select_color(Clubs, Orbit)
		])
	]), !.
infoPage_just_this_orbit_OrbitColor(_, _, _) --> [], !.


infoPage_orbit_select_color(0, _Orbit) -->
	html(&(nbsp)), !.
infoPage_orbit_select_color(NumberOfClubs, Orbit) -->
	{
		NumberOfClubs > 0,
		atom_concat(color, Orbit, SelectName),
		findall([Number, Name], jp_Color([number(Number), name(Name)]), SelectList)
	},
	html(
		select([name(SelectName), size(1)],[
			\infoPage_orbit_select_color_option(Orbit, SelectList)
		])
	).

infoPage_orbit_select_color_option(_, []) --> [], !.
infoPage_orbit_select_color_option(Orbit, [[Number, Name]|SelectList]) -->
	html_option(Number, Orbit, Name),
	infoPage_orbit_select_color_option(Orbit, SelectList).

	
	
%%% --- juggler info --- %%%
	

infoPage_juggler_info([], _ActionList, SwapList, _ClubDistribution, NumberOfJugglers, _Period, Pattern, BackURL) -->
	{
		JugglerMax is NumberOfJugglers - 1,
		numlist(0, JugglerMax, ListOfJugglers),
		applyNewSwaps(SwapList, ListOfJugglers, NewSwapList)
	},
	html(
		div([class(infoPage_swap_all_link)],[
			\html_href(Pattern, NumberOfJugglers, NewSwapList, BackURL, [class(small)], 'swap all hands')
		])
	), !.
infoPage_juggler_info([Juggler|ListOfJugglers], ActionList, SwapList, ClubDistribution, NumberOfJugglers, Period, Pattern, BackURL) -->
	{
		ColspanLong is Period,
		ColspanShort is Period - 1,
		jugglerShown(Juggler, JugglerShown),
		nth0(Juggler, ClubDistribution, [ClubsHandA, ClubsHandB]),
		handShown(Juggler, a, SwapList, HandShownA),
		handShownLong(HandShownA, HandShownALong),
		handShown(Juggler, b, SwapList, HandShownB),
		handShownLong(HandShownB, HandShownBLong),
		applyNewSwaps(SwapList, [Juggler], NewSwapList)
	},
	html([
		table([class(info_juggler_table)],[
			tr([],[
				td([class(info_swaplink)],[
					\html_href(Pattern, NumberOfJugglers, NewSwapList, BackURL, [class(small)], 'swap hands')
				]),
				th([class(info_title), colspan(ColspanLong)],[
					'juggler ', JugglerShown
				])
			]),
			\infoPage_clubs_in_hand(right, HandShownALong, ClubsHandA, HandShownBLong, ClubsHandB, ColspanShort),
			\infoPage_clubs_in_hand(left, HandShownALong, ClubsHandA, HandShownBLong, ClubsHandB, ColspanShort),
			tr([],[
				td([class(info_lable)],[
					'throwing hand:'
				]),
				\infoPage_jugglers_throwing_hand(Juggler, ActionList, SwapList)
			]),
			tr([],[
				td([class(info_lable)],[
					'throw:'
				]),
				\infoPage_jugglers_throw(Juggler, ActionList, NumberOfJugglers, Period)
			]),
			tr([],[
				td([class(info_lable)],[
					'cross/tramline:'
				]),
				\infoPage_jugglers_cross_tramline(Juggler, ActionList, SwapList)
			]),
			tr([],[
				td([class(info_lable)],[
					'catching hand:'
				]),
				\infoPage_jugglers_catching_hand(Juggler, ActionList, SwapList)
			]),
			\infoPage_jugglers_no_multiplex_info(Juggler, ActionList, NumberOfJugglers, Period, Pattern)
		])
	]),	
	infoPage_juggler_info(ListOfJugglers, ActionList, SwapList, ClubDistribution, NumberOfJugglers, Period, Pattern, BackURL).
	
infoPage_clubs_in_hand(Hand, Hand, Clubs, _WrongHand, _WrongClubs, Colspan) -->
	infoPage_clubs_in_hand(Hand, Clubs, Colspan), !.
infoPage_clubs_in_hand(Hand, _WrongHand, _WrongClubs, Hand, Clubs, Colspan) -->
	infoPage_clubs_in_hand(Hand, Clubs, Colspan), !.

infoPage_clubs_in_hand(Hand, Clubs, Colspan) -->
	html(
		tr([],[
			td([class(info_lable)],[
				'clubs in ', Hand, ' hand:'
			]),
			td([class(info_clubs)],[
				Clubs
			]),
			td([class(info_clubs), colspan(Colspan)],[
				&(nbsp)
			])
		])
	).


	
infoPage_jugglers_throwing_hand(_ThrowingJuggler, [], _SwapList) --> [], !.
infoPage_jugglers_throwing_hand(ThrowingJuggler, [Action|ActionList], SwapList) -->
	{
		nth1(2, Action, ThrowingJuggler),!,
		nth1(3, Action, ThrowingSiteswapPosition),
		hand(ThrowingSiteswapPosition, Hand),
		handShown(ThrowingJuggler, Hand, SwapList, HandShown)
	},
	html([
		td([class(info_hand)],[
			HandShown
		])
	]),
	infoPage_jugglers_throwing_hand(ThrowingJuggler, ActionList, SwapList).
infoPage_jugglers_throwing_hand(ThrowingJuggler, [_Action|ActionList], SwapList) -->
	infoPage_jugglers_throwing_hand(ThrowingJuggler, ActionList, SwapList), !.
	
	

infoPage_jugglers_throw(_ThrowingJuggler, [], _NumberOfJugglers, _Period) --> [], !.
infoPage_jugglers_throw(ThrowingJuggler, [Action|ActionList], NumberOfJugglers, Period) -->
	{
		nth1(2, Action, ThrowingJuggler),!,
		nth1(4, Action, Throw)
	},
	html([
		td([class(info_throw)],[
			\html_throw(Throw, [hideIndex(NumberOfJugglers), colorThrow(Period), absoluteIndex(NumberOfJugglers, ThrowingJuggler)])
		])
	]),
	infoPage_jugglers_throw(ThrowingJuggler, ActionList, NumberOfJugglers, Period).
infoPage_jugglers_throw(ThrowingJuggler, [_Action|ActionList], NumberOfJugglers, Period) -->
	infoPage_jugglers_throw(ThrowingJuggler, ActionList, NumberOfJugglers, Period), !.

	
infoPage_jugglers_cross_tramline(_ThrowingJuggler, [], _SwapList) --> [], !.
infoPage_jugglers_cross_tramline(ThrowingJuggler, [Action|ActionList], SwapList) -->
	{
		nth1(2, Action, ThrowingJuggler),!
	},
	html([
		td([class(info_cross)],[
			\infoPage_the_cross(ThrowingJuggler, Action, SwapList)
		])
	]),
	infoPage_jugglers_cross_tramline(ThrowingJuggler, ActionList, SwapList).
infoPage_jugglers_cross_tramline(ThrowingJuggler, [_Action|ActionList], SwapList) -->
	infoPage_jugglers_cross_tramline(ThrowingJuggler, ActionList, SwapList), !.



infoPage_the_cross(ThrowingJuggler, Action, _SwapList) -->
	{
		nth1(6, Action, ThrowingJuggler),! % self
	},
	html(&(nbsp)).
infoPage_the_cross(ThrowingJuggler, Action, SwapList) -->
	{
		nth1(6, Action, CatchingJugglers),
		is_list(CatchingJugglers), !,  % Multiplex
		nth1(3, Action, ThrowingSiteswapPosition),
		nth1(7, Action, CatchingSiteswapPositions),
		hand(ThrowingSiteswapPosition, ThrowingHand),
		hand(CatchingSiteswapPositions, CatchingHands),
		handShown(ThrowingJuggler, ThrowingHand, SwapList, ThrowingHandShown),
		handShown(CatchingJugglers, CatchingHands, SwapList, CatchingHandsShown),
		findall(
			CrossOrTram,
			(
				nth0(Pos, CatchingJugglers, CatchingJuggler),
				nth0(Pos, CatchingHandsShown, CatchingHandShown),
				cross_or_tramline(ThrowingJuggler, CatchingJuggler, ThrowingHandShown, CatchingHandShown, CrossOrTram)
		),
		CrossOrTramList
		)
	},
	html_list(CrossOrTramList, []).
infoPage_the_cross(ThrowingJuggler, Action, SwapList) -->
	{
		nth1(3, Action, ThrowingSiteswapPosition),
		nth1(6, Action, CatchingJuggler),
		nth1(7, Action, CatchingSiteswapPosition),
		hand(ThrowingSiteswapPosition, ThrowingHand),
		hand(CatchingSiteswapPosition, CatchingHand),
		handShown(ThrowingJuggler, ThrowingHand, SwapList, ThrowingHandShown),
		handShown(CatchingJuggler, CatchingHand, SwapList, CatchingHandShown)
	},
	infoPage_cross_or_tramline(ThrowingJuggler, CatchingJuggler, ThrowingHandShown, CatchingHandShown).


infoPage_cross_or_tramline(Juggler, Juggler, _HandA, _HandB) -->
 	html(&(nbsp)),!.
infoPage_cross_or_tramline(_TJuggler, _CJuggler, Hand, Hand) -->
	html('X'),!.
infoPage_cross_or_tramline(_TJuggler, _CJuggler, _HandA, _HandB) -->
	 html('||'),!.

cross_or_tramline(Juggler, Juggler, _HandA, _HandB, &(nbsp)) :- !.
cross_or_tramline(_TJuggler, _CJuggler, Hand, Hand, 'X') :- !.
cross_or_tramline(_TJuggler, _CJuggler, _HandA, _HandB, '||') :- !.

infoPage_jugglers_catching_hand(_ThrowingJuggler, [], _SwapList) --> [], !.
infoPage_jugglers_catching_hand(ThrowingJuggler, [Action|ActionList], SwapList) -->
	{
		nth1(2, Action, ThrowingJuggler),!,
		nth1(6, Action, CatchingJuggler),
		nth1(7, Action, CatchingSiteswapPosition),
		hand(CatchingSiteswapPosition, Hand),
		handShown(CatchingJuggler, Hand, SwapList, HandShown)
	},
	html([
		td([class(info_hand)],[
			\html_list(HandShown, [])
		])
	]),
	infoPage_jugglers_catching_hand(ThrowingJuggler, ActionList, SwapList).
infoPage_jugglers_catching_hand(ThrowingJuggler, [_Action|ActionList], SwapList) -->
	infoPage_jugglers_catching_hand(ThrowingJuggler, ActionList, SwapList), !.

	
infoPage_jugglers_no_multiplex_info(Juggler, ActionList, NumberOfJugglers, Period, Pattern) -->
	{
		noMultiplex(Pattern)
	},
	html([
		tr([],[
			td([class(info_lable)],[
				'was:'
			]),
			\infoPage_jugglers_throw_was(Juggler, ActionList, NumberOfJugglers, Period, Pattern)
		]),
		tr([],[
			td([class(info_lable)],[
				'caused by:'
			]),
			\infoPage_jugglers_throw_caused_by(Juggler, ActionList, NumberOfJugglers, Period, Pattern)
		])
	]).
infoPage_jugglers_no_multiplex_info(_Juggler, _ActionList, _NumberOfJugglers, _Period, _Pattern) -->
	[], !.
	

infoPage_jugglers_throw_was(_ThrowingJuggler, [], _NumberOfJugglers, _Period, _Pattern) --> [], !.
infoPage_jugglers_throw_was(ThrowingJuggler, [Action|ActionList], NumberOfJugglers, Period, Pattern) -->
	{
		nth1(2, Action, ThrowingJuggler),!,
		nth1(3, Action, SiteswapPosition),
		throw_was(Pattern, SiteswapPosition, ThrowPosition),
		nth0(ThrowPosition, Pattern, Throw)
	},
	html([
		td([class(info_throw)],[
			\html_throw(Throw, [hideIndex(NumberOfJugglers), colorThrow(Period)])
		])
	]),
	infoPage_jugglers_throw_was(ThrowingJuggler, ActionList, NumberOfJugglers, Period, Pattern).
infoPage_jugglers_throw_was(ThrowingJuggler, [_Action|ActionList], NumberOfJugglers, Period, Pattern) -->
	infoPage_jugglers_throw_was(ThrowingJuggler, ActionList, NumberOfJugglers, Period, Pattern), !.


infoPage_jugglers_throw_caused_by(_ThrowingJuggler, [], _NumberOfJugglers, _Period, _Pattern) --> [], !.
infoPage_jugglers_throw_caused_by(ThrowingJuggler, [Action|ActionList], NumberOfJugglers, Period, Pattern) -->
	{
		nth1(2, Action, ThrowingJuggler),!,
		nth1(3, Action, SiteswapPosition),
		throw_reacts_to(Pattern, SiteswapPosition, ThrowPosition),
		nth0(ThrowPosition, Pattern, Throw)
	},
	html([
		td([class(info_throw)],[
			\html_throw(Throw, [hideIndex(NumberOfJugglers), colorThrow(Period)])
		])
	]),
	infoPage_jugglers_throw_caused_by(ThrowingJuggler, ActionList, NumberOfJugglers, Period, Pattern).
infoPage_jugglers_throw_caused_by(ThrowingJuggler, [_Action|ActionList], NumberOfJugglers, Period, Pattern) -->
	infoPage_jugglers_throw_caused_by(ThrowingJuggler, ActionList, NumberOfJugglers, Period, Pattern), !.



%%% --- joepass link --- %%%

infoPage_joepass_link(Pattern, Persons, SwapList, Request) -->
	{
		get_cookie(joepass_download, Request, JoePass_Download),
		get_cookie(joepass_style, Request, JoePass_Style),
		get_cookie(joepass_file, Request, JoePass_File),
        JoePass_DistanceMax is Persons - 1,
		float_to_shortpass(Pattern, PatternShort),
		jp_filename(PatternShort, FileName),
		atom_concat(FileName, '.pass', FileNamePass),
		
		atom2Pattern(PatternAtom, Pattern),	
		atom2SwapList(SwapListAtom, SwapList),
		www_form_encode(PatternAtom, PatternEnc),
		www_form_encode(SwapListAtom, SwapListEnc),
		www_form_encode(Persons, PersonsEnc),
		www_form_encode(FileName, FileNameEnc)
	},
	html([
		div([class(jp_link)],[
			input([type(hidden), name(pattern), value(PatternEnc)]),
			input([type(hidden), name(persons), value(PersonsEnc)]),
			input([type(hidden), name(filename), value(FileNameEnc)]),
			input([type(hidden), name(swap), value(SwapListEnc)]),
			a([href('http://www.koelnvention.de/software'), target('_blank')],['JoePass!']),
			&(nbsp),
			'file:',
			&(nbsp),
			select([name(download), size(1)],[
				\html_option('on', JoePass_Download, 'download'),
				\html_option('off', JoePass_Download, 'show')
			]),
			&(nbsp),
			select([name(nametype), size(1)],[
				\html_option('joe', JoePass_File, 'joe.pass'),
				\html_option('numbers', JoePass_File, FileNamePass)
			]),
			&(nbsp),
			select([name(style), size(1)],[
				\html_option('none', JoePass_Style, &(nbsp)),
				\html_option('normal', JoePass_Style, 'face to face'),
				\html_option('shortdistance', JoePass_Style, 'short distance'),
				\html_option('sidebyside', JoePass_Style, 'side by side'),
				\html_option('backtoback', JoePass_Style, 'back to back'),
				\html_option('dropbackline', JoePass_Style, 'drop back line')
			]),
			&(nbsp),
			select([name(distance), size(1)],[
                \html_numbered_options(1, JoePass_DistanceMax, 1)
			]),
			&(nbsp),
			input([type(submit), value('go')])
		])
	]).



%%% --- hidden js info --- %%%


infoPage_hidden_info(Pattern, Persons, SwapList, BackURL) -->
	{		
		flatten(Pattern, PatternFlat),
		length(PatternFlat, NumberOfNumbers),
		atom2Pattern(PatternAtom, Pattern),	
		atom2SwapList(SwapListAtom, SwapList),
		www_form_encode(PatternAtom, PatternEnc),
		www_form_encode(SwapListAtom, SwapListEnc),
		www_form_encode(Persons, PersonsEnc),
		www_form_encode(BackURL, BackURLEnc)
	},
	html([
		form([id(info_form), action('.'), method(post)],[
			input([type(hidden), name(pattern), value(PatternEnc)]),
			input([type(hidden), name(persons), value(PersonsEnc)]),
			input([type(hidden), name(swap), value(SwapListEnc)]),
			input([type(hidden), name(numbers), value(NumberOfNumbers)]),
			input([type(hidden), name(back), value(BackURLEnc)])
		])
	]).

% --------- %



html_href(Pattern, Persons, SwapList, BackURL, Attributes, Content) -->
	{
		atom2Pattern(PatternAtom, Pattern),	
		atom2SwapList(SwapListAtom, SwapList),
		%www_form_encode(PatternAtom, PatternEnc),
		%www_form_encode(SwapListAtom, SwapListEnc),
		%www_form_encode(Persons, PersonsEnc),
		%www_form_encode(BackURL, BackURLEnc),
		
		parse_url_search(Search, [pattern(PatternAtom), persons(Persons), swap(SwapListAtom), back(BackURL)]),
		http_info_page_path(Path),
		format(atom(Href), ".~w?~s", [Path, Search])
	},
	html_href(Href, Attributes, Content).


