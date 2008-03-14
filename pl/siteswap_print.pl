
allSiteswaps(Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, MaxNumberOfResults, BackURL) :-
   get_time(Start),
   findAtMostNUnique(Throws, 
         siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React),
         MaxNumberOfResults,
         Bag,
         Flag),!,
   sortListOfSiteswaps(Bag,Swaps),
   length(Swaps, NumberOfResults),
   (Flag = some -> 
      format("<p class='some'>Just a selection of patterns is shown!</p>");
      (NumberOfResults is 1 ->	
         format("<p class='all'>The only possible pattern has been found!</p>");
         format("<p class='all'>All ~w patterns have been found!</p>", [NumberOfResults])
      )		
   ),
   write_errors("<p class='error'>~w</p>"),
   get_time(End),
   Time is End - Start,
   format("<p class='time'>(~w seconds)</p>\n", [Time]),
   forall(member(T, Swaps),  writePassingSwap(T, Persons, BackURL)).


cleanListOfSiteswaps(List,CleanList) :-
   cleanEquals(List, Swaps),
   rotateAll(Swaps,SwapsRotated),
   sortListOfSiteswaps(SwapsRotated, CleanList).

sortListOfSiteswaps(Swaps, SwapsSorted) :-
   addKeys(Swaps,SwapsWithKeys),
   keysort(SwapsWithKeys,SwapsSortedWithKeys),
   removeKeys(SwapsSortedWithKeys,SwapsSorted).


addKeys([],[]).
addKeys([Head|Swaps],[Key-Head|SwapsWithKeys]) :-
	%%listOfHeights(Head,Heights),
	amountOfPasses(Head,Number),
	rat2float(Head,HeadFloat),
	Key = [Number,HeadFloat],
	addKeys(Swaps,SwapsWithKeys).

removeKeys([],[]).
removeKeys([_Key-Head|SwapsWithKeys],[Head|Swaps]) :-
	removeKeys(SwapsWithKeys,Swaps).

convertP([], [], _, _).
convertP([p(FirstThrow,Index,Origen) | RestThrows ], [  FirstThrowP| RestThrowsP], Length, Persons) :- 
	Index > 0,
	pStyle(Length, Origen, Style),
	float_to_shortpass(FirstThrow,FirstThrowShort),
	((
		Persons > 2,
		sformat(FirstThrowP, "<span class='~w'>~wp<sub>~w</sub></span>", [Style,FirstThrowShort,Index])
	);
	(
		Persons = 2,
		sformat(FirstThrowP, "<span class='~w'>~wp</span>", [Style,FirstThrowShort])
	)),
    convertP(RestThrows, RestThrowsP, Length, Persons).
convertP([ p(FirstThrow, 0, _Origen)  | RestThrows ], [ FirstThrow | RestThrowsP], Length, Persons) :-
    number(FirstThrow),
	%%atom_concat(FirstThrow, '', FirstThrowP),
    convertP(RestThrows, RestThrowsP, Length, Persons).
convertP([ FirstThrow | RestThrows ], [ FirstThrowP | RestThrowsP], Length, Persons) :-
    is_list(FirstThrow),
	convertP(FirstThrow,FirstThrowP, Length, Persons),
    convertP(RestThrows, RestThrowsP, Length, Persons).

%% single throw:
convertP(p(Throw, Index, Origen), ThrowP, Length, Persons) :-
	convertP([p(Throw, Index, Origen)], [ThrowP], Length, Persons).

convertP(Throw, ThrowP, Length, Persons) :-
	number(Throw),
	convertP([Throw], [ThrowP], Length, Persons).


%% single throw version needed!!!
convertMultiplex([],[]).
convertMultiplex([Multiplex | Rest], [MultiplexNew | RestNew]) :-
    is_list(Multiplex), !,
 	concat_atom(Multiplex, ',', MultiplexTemp),
 	atom_concat('[',MultiplexTemp,MultiplexTemp2),
 	atom_concat(MultiplexTemp2, ']', MultiplexNew),
 	convertMultiplex(Rest, RestNew).
convertMultiplex([Throw | Rest], [Throw | RestNew]) :-
	convertMultiplex(Rest, RestNew).

writeSwap(ThrowsPM, Throws, Persons, BackURL) :-
   concat_atom(ThrowsPM, ' ', Swap),
   float_to_shortpass(Throws,ThrowsShort),
   format("<a href='./info.php?pattern=~w&persons=~w&back=~w'>~w</a><br>\n", [ThrowsShort,Persons,BackURL,Swap]),!.

writePassingSwap(Throws, Persons, BackURL) :-
	length(Throws,Length),
    convertP(Throws, ThrowsP, Length, Persons),
	convertMultiplex(ThrowsP,ThrowsPM),
    writeSwap(ThrowsPM, Throws, Persons, BackURL).

writeCompletedSiteswap(Pattern, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, BackURL) :-
   siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React),
   rotateHighestFirst(Throws, Pattern),
   writePassingSwap(Pattern, Persons, BackURL).




pStyle(Length, Origen, classic) :- 
	even(Length),
	odd(Origen).
pStyle(Length, Origen, equi) :-
	even(Length),
	even(Origen).
pStyle(Length, Origen, bi) :-
	odd(Length),
	even(Origen).
pStyle(Length, Origen, instantbi) :-
	odd(Length),
	odd(Origen).

