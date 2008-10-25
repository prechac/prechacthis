


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
		float_to_shortpass(Throws, ThrowsShort)
		%magicPositions(Throws, Persons, MagicPositions)
	},	
	html([
		p([],[
			\html_href(ThrowsShort, Persons, [], BackURL, [], \mainPage_siteswap(ThrowsShort, Length, Persons, []))
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
	html_throw(Throw, Position, Length, Persons, MagicPositions, relative),!.
mainPage_siteswap([Throw|RestThrows], Length, Persons, MagicPositions) -->
	{
		length(RestThrows, RestLength),
		Position is Length - RestLength - 1
	},
	html_throw(Throw, Position, Length, Persons, MagicPositions, relative),
	html(&(nbsp)),
	mainPage_siteswap(RestThrows, Length, Persons, MagicPositions).
	
	
html_throw(Throw, _Position, _Length, _Persons, _MagicPositions, _ThrowingJuggler) -->
	{
		is_list(Throw), !
	},
	html('multiplex').
html_throw(Throw, _Position, _Length, Persons, _MagicPositions, ThrowingJuggler) -->
	html_throw_Content(Throw, Persons, ThrowingJuggler).
	

html_throw_Content(p(Throw, 0, _Origen), _Persons, _ThrowingJuggler) -->
	html(Throw), !.
html_throw_Content(p(Throw, _Index, _Origen), Persons, _ThrowingJuggler) -->
	{
		number(Persons),
		Persons < 3, !,
		float_to_shortpass(Throw, ThrowShort),
		a2Atom(ThrowShort, ThrowAtom)
	},
	html([ThrowAtom, 'p']).
html_throw_Content(p(Throw, Index, _Origen), Persons, ThrowingJuggler) -->
	{
		number(ThrowingJuggler), !,
		CatchingJuggler is (ThrowingJuggler + Index) mod Persons,
		jugglerShown(CatchingJuggler, PrintIndex),
		float_to_shortpass(Throw, ThrowShort),
		a2Atom(ThrowShort, ThrowAtom)
	},
	html([ThrowAtom, 'p', sub([], [PrintIndex])]).



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

