
allSiteswaps(Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic, MaxNumberOfResults, BackURL) :-
   catch(
   (
      get_time(Start),
      findAtMostNUnique(Throws, 
         siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, Magic),
         MaxNumberOfResults, 
         Bag,
         Flag
      ),
      !,
      sortListOfSiteswaps(Bag,Swaps),
      length(Swaps, NumberOfResults),
      (Flag = some -> 
         format("<p class='some'>Just a selection of patterns is shown!</p>");
         (NumberOfResults is 1 ->	
            format("<p class='all'>The only possible pattern has been found!</p>");
            format("<p class='all'>All ~w patterns have been found!</p>", [NumberOfResults])
         )		
      ),
      get_time(End),
      Time is End - Start,
      format("<p class='time'>(~w seconds)</p>\n", [Time]),
      forall(member(T, Swaps),  writePassingSwap(T, Persons, BackURL))
   ),
   Exception,
   print_exception(Exception)
   ).

print_exception(constraint_unclear(Constraint)) :-
  format("<p class='exception'>Sorry, your constraint ~w is unclear.</p>", [Constraint]),
  !,fail.
print_exception(constraint_unclear) :-
  format("<p class='exception'>Sorry, your constraints are unclear.</p>"),
  !,fail.
print_exception(Exception) :-
	format(Exception),
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
   pattern_to_string(ThrowsShort, URLPattern),
   format("<a href='./info.php?pattern=~s&persons=~w&back=~w'>~w</a><br>\n", [URLPattern,Persons,BackURL,Swap]),!.

writePassingSwap(Throws, Persons, BackURL) :-
	length(Throws, Length),
	magicPositions(Throws, Persons, MagicPositions),
    convertP(Throws, ThrowsP, Length, Persons),
	convertMagic(ThrowsP, MagicPositions, ThrowsPM),
	convertMultiplex(ThrowsPM,ThrowsPMM),
    writeSwap(ThrowsPMM, Throws, Persons, BackURL).

writeCompletedSiteswap(Pattern, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, BackURL) :-
   siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React),
   rotateHighestFirst(Throws, Pattern),
   writePassingSwap(Pattern, Persons, BackURL).


pattern_to_string(Pattern, PatternStr) :-
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

