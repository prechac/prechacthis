


print_exception(Constraint) :-
  format("<p class='exception'>Sorry, your constraint ~w is unclear.</p>", [Constraint]),
  !,fail.


convertP(Throws, ThrowsP, Length, Persons) :-
	convertP(Throws, ThrowsP, Length, Persons, relative).

convertP([], [], _, _, _).
convertP([p(FirstThrow,Index,Origen) | RestThrows ], [  FirstThrowP| RestThrowsP], Length, Persons, ThrowingJuggler) :- 
	Index > 0,
	pStyle(Length, Origen, Index, Style),
	float_to_shortpass(FirstThrow,FirstThrowShort),
	(number(ThrowingJuggler) ->
		(
			CatchingJuggler is (ThrowingJuggler + Index) mod Persons,
			jugglerShown(CatchingJuggler, PrintIndex)
		);
		PrintIndex = Index
	),
	((
		Persons > 2,
		format(string(FirstThrowP), "<span class='~w'>~wp<sub>~w</sub></span>", [Style,FirstThrowShort,PrintIndex])
	);
	(
		Persons = 2,
		format(string(FirstThrowP), "<span class='~w'>~wp</span>", [Style,FirstThrowShort])
	)),
    convertP(RestThrows, RestThrowsP, Length, Persons, ThrowingJuggler).
convertP([ p(FirstThrow, 0, _Origen)  | RestThrows ], [ FirstThrowP | RestThrowsP], Length, Persons, Juggler) :-
    number(FirstThrow),
	format(string(FirstThrowP), "~w", [FirstThrow]),
	%%atom_concat(FirstThrow, '', FirstThrowP),
    convertP(RestThrows, RestThrowsP, Length, Persons, Juggler).
convertP([ FirstThrow | RestThrows ], [ FirstThrowP | RestThrowsP], Length, Persons, Juggler) :-
    is_list(FirstThrow),
	convertP(FirstThrow,FirstThrowP, Length, Persons, Juggler),
    convertP(RestThrows, RestThrowsP, Length, Persons, Juggler).

%% single throw:
convertP(p(Throw, Index, Origen), ThrowP, Length, Persons, Juggler) :-
	convertP([p(Throw, Index, Origen)], [ThrowP], Length, Persons, Juggler).

convertP(Throw, ThrowP, Length, Persons, Juggler) :-
	number(Throw),
	convertP([Throw], [ThrowP], Length, Persons, Juggler).

	
identifyThrows(Throws, ThrowsID) :-
	identifyThrows(Throws, 0, ThrowsID).
identifyThrows([], _, []) :- !.
identifyThrows([Multiplex|Throws], ID, [MultiplexID|ThrowsID]) :-
	is_list(Multiplex),
	identifyThrows(Multiplex, ID, MultiplexID),
	flatten(Multiplex, MultiplexFlat),
	length(MultiplexFlat, Length),
	NextID is ID + Length,
	identifyThrows(Throws, NextID, ThrowsID).
identifyThrows([Throw|Throws], ID, [ThrowID|ThrowsID]) :-
	format(string(ThrowID), "<span id='throw~w'>~s</span>", [ID, Throw]),
	NextID is ID + 1,
	identifyThrows(Throws, NextID, ThrowsID).


convertMagic(Pattern, [], Pattern) :- !.
convertMagic(Pattern, [Pos|MagicPositions], MagicPattern) :- 
	nth0ListOfLists(Pos, Pattern, Throw),
	format(string(MagicThrow), "<span class='magic' title='it&#39;s&nbsp;magic'>~s</span>", [Throw]),
	changeOnePosition(Pattern, Pos, MagicThrow, TmpMagicPattern),
	convertMagic(TmpMagicPattern, MagicPositions, MagicPattern).


%% single throw version needed!!!
convertMultiplex(Multiplex, MultiplexNew) :-
	convertMultiplex(Multiplex, MultiplexNew, ' ').
convertMultiplex([],[], _Space).
convertMultiplex([Multiplex | Rest], [MultiplexNew | RestNew], Space) :-
    is_list(Multiplex), !,
 	concat_atom(Multiplex, Space, MultiplexTemp),
 	atom_concat('[',MultiplexTemp,MultiplexTemp2),
 	atom_concat(MultiplexTemp2, ']', MultiplexNew),
 	convertMultiplex(Rest, RestNew, Space).
convertMultiplex([Throw | Rest], [Throw | RestNew], Space) :-
	convertMultiplex(Rest, RestNew, Space).
	

writeSwap(ThrowsPM, Throws, Persons, BackURL) :-
   concat_atom(ThrowsPM, ' ', Swap),
   float_to_shortpass(Throws,ThrowsShort),
   format("<a href="),
   format_href(ThrowsShort, Persons, [], BackURL),
   format(">~w</a><br>\n", [Swap]),!.


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
	

	
	
/*** html_throw(Throw, Options)
*    Options:
*       colorThrow(Length),
*       hideIndex(Persons),
*       absoluteIndex(Persons, ThrowingJuggler),
*       magic(Position, MagicPositions),
*       id(Prefix),
*/	
html_throw(Multiplex, Options) -->
	html_list(Multiplex, [throw|Options]).
	
	
html_throw_process_options(Throw, Options) -->
	html_throw_process_options(Throw, Options, Options).

html_throw_process_options(Throw, [], OriginalOptions) -->
	html_throw_content(Throw, OriginalOptions), !.
html_throw_process_options(Throw, [Option|Options], OriginalOptions) -->
	html_throw_option(Throw, Option, \html_throw_process_options(Throw, Options, OriginalOptions)).
	
	
	
html_throw_option(p(_Throw,0,_Origen), colorThrow(_Length), InnerHTML) -->
	html(InnerHTML), !.
html_throw_option(Throw, colorThrow(Length), InnerHTML) -->
	{
		Throw = p(_, Index, Origen),
		Index \= 0,
		pStyle(Length, Origen, Index, Style)
	},
	html(
		span([class(Style)],[InnerHTML])
	),!.
html_throw_option(_Throw, magic(Position, MagicPositions), InnerHTML) -->
	{
		memberchk(Position, MagicPositions)
	},
	html(
		span([class(magic), title('it\'s magic')],[InnerHTML])
	),!.
html_throw_option(_Throw, id(Prefix), InnerHTML) -->
	{
		html_throw_id_next(Prefix, Next),
		atom_concat(Prefix, Next, ID)
	},
	html(
		span([id(ID)],[InnerHTML])
	),!.
html_throw_option(_Throw, _Option, InnerHTML) -->
	html(InnerHTML).


html_throw_content(p(Throw, 0, _Origen), _Options) -->
	{
		ThrowPrint is round(Throw)		
	},
	html(ThrowPrint), !.
html_throw_content(p(Throw, Index, _Origen), Options) -->
	{
		Index \= 0,
		memberchk(hideIndex(Persons), Options),
		number(Persons),
		Persons < 3, !,
		float_to_shortpass(Throw, ThrowShort),
		a2Atom(ThrowShort, ThrowAtom)
	},
	html([ThrowAtom, 'p']).
html_throw_content(p(Throw, Index, _Origen), Options) -->
	{
		Index \= 0,
		memberchk(absoluteIndex(Persons, ThrowingJuggler), Options),
		number(ThrowingJuggler),
		number(Persons), !,
		CatchingJuggler is (ThrowingJuggler + Index) mod Persons,
		jugglerShown(CatchingJuggler, PrintIndex),
		float_to_shortpass(Throw, ThrowShort),
		a2Atom(ThrowShort, ThrowAtom)
	},
	html([ThrowAtom, 'p', sub([], [PrintIndex])]).
html_throw_content(p(Throw, Index, _Origen), _Options) -->
	{
		Index \= 0,
		float_to_shortpass(Throw, ThrowShort),
		a2Atom(ThrowShort, ThrowAtom),
		a2Atom(Index, IndexAtom)
	},
	html([ThrowAtom, 'p', sub([], [IndexAtom])]).



init_html_throw_id :-
	forall(recorded(html_throw_id, _, R), erase(R)).


html_throw_id_next(Prefix, Next) :-
	recorded(html_throw_id, id(Prefix, Last), LastRef),
	Next is Last + 1,
	erase(LastRef),
	recorda(html_throw_id, id(Prefix, Next)), !.
html_throw_id_next(Prefix, 0) :-
	recorda(html_throw_id, id(Prefix, 0)).
	





/*** html_list(List, Options)
*    Options:
*       td(Attributes)
*       list_seperator(Seperator)
*       list_left(Left)
*       list_right(Right)
*       befor(Befor)
*       after(After)
*/	
html_list(List, Options) -->
	{
		is_list(List), !
	},
	html_list_first_middle_last(List, Options).
html_list(NoList, Options) -->
	html_list_process_element_options(NoList, Options).


html_list_first_middle_last(List, Options) -->
	{
		append([First|Middle], [Last], List)
	},
	html_list_first(First, Options),
	html_list_seperator(Options),
	html_list_content(Middle, Options),
	html_list_last(Last, Options).


html_list_first(First, Options) -->
	{
		append(Options, [befor(\html_list_left(Options))], OptionsFirst)
	},
	html_list(First, OptionsFirst).

html_list_last(Last, Options) -->
	{
		append(Options, [after(\html_list_right(Options))], OptionsLast)
	},
	html_list(Last, OptionsLast).

html_list_content([], _Options) -->
	[], !.
html_list_content([Element], Options) --> 
	html_list(Element, Options), !.
html_list_content([Element|List], Options) -->
	html_list(Element, Options),
	html_list_seperator(Options),
	html_list_content(List, Options).


html_list_process_element_options(Throw, Options) -->
	html_list_process_element_options(Throw, Options, Options).

html_list_process_element_options(Element, [], OriginalOptions) -->
	html_list_element(Element, OriginalOptions), !.
html_list_process_element_options(Element, [Option|Options], OriginalOptions) -->
	html_list_element_option(Element, Option, \html_list_process_element_options(Element, Options, OriginalOptions)).



html_list_element_option(_Element, td(Attributes), InnerHTML) -->
	html(
		td(Attributes, [InnerHTML])
	),!.
html_list_element_option(_Element, befor(Befor), InnerHTML) -->
	html([
		Befor,
		InnerHTML
	]),!.
html_list_element_option(_Element, after(After), InnerHTML) -->
	html([
		InnerHTML,
		After
	]),!.
html_list_element_option(_Element, _Option, InnerHTML) -->
	html(InnerHTML).
	
	
	
html_list_element(Throw, Options) -->
	{
		memberchk(throw, Options), !
	},
	html_throw_process_options(Throw, Options).
html_list_element(Element, _Options) -->
	html(Element).


html_list_seperator(Options) -->
	{
		memberchk(td(_), Options), !
	},
	[].
html_list_seperator(Options) -->
	{
		memberchk(list_seperator(Seperator), Options),!
	},
	html([Seperator]).
html_list_seperator(_Options) -->
	html(&(nbsp)).


html_list_left(Options) -->
	{
		memberchk(list_left(Left), Options), !
	},
	html([Left]).
html_list_left(_Options) -->
	html('['),!.


html_list_right(Options) -->
	{
		memberchk(list_right(Right), Options), !
	},
	html([Right]).
html_list_right(_Options) -->
	html(']'),!.





%%  4 <span class='magic' title='it&#39;s&nbsp;magic'><span class='equi'>2p</span></span> 0 <span class='magic' title='it&#39;s&nbsp;magic'><span class='equi'>2p</span></span>

writeCompletedSiteswap(Pattern, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, BackURL) :-
   siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React),
   rotateHighestFirst(Throws, Pattern),
   writePassingSwap(Pattern, Persons, BackURL).


pattern_to_string(Pattern, PatternStr) :-
	list_to_string(Pattern, PatternStr).
list_to_string(Pattern, PatternStr) :-
	format(atom(TempStr), "~w", [Pattern]),
	string_to_list(TempStr, TempLst),
	remove_whitespace(TempLst, PatternStr).




pStyle(_Length, _Origen, 0, self) :- !.
pStyle(Length, Origen, _Index, classic) :- 
	even(Length),
	odd(Origen).
pStyle(Length, Origen, _Index, equi) :-
	even(Length),
	even(Origen).
pStyle(Length, Origen, Index, bi) :-
	odd(Length),
	even(Origen), 
	odd(Index).
pStyle(Length, Origen, Index, bi) :-
	odd(Length),
	odd(Origen), 
	even(Index).
pStyle(Length, Origen, Index, instantbi) :-
	odd(Length),
	even(Origen),
	even(Index).
pStyle(Length, Origen, Index, instantbi) :-
	odd(Length),
	odd(Origen),
	odd(Index).

