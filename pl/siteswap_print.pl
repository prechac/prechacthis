allPassingSiteswaps(Persons, Objects, Lengths, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React) :-
   get_time(Start),
   findall(Throws, 
         passing_siteswap(Throws, Persons, Objects, Lengths, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React),
         Bag),
   cleanListOfSiteswaps(Bag,Swaps),
   get_time(End),
   Time is End - Start,
   format("<p class='time'>(~w seconds)</p>\n", [Time]),
   forall(member(T, Swaps),  writePassingSwap(T, Persons)).

atom_throws(ThrowsAtom, Throws) :-
    concat_atom(List, ' ', ThrowsAtom),
    numerizeList(List, Throws).


cleanListOfSiteswaps(List,CleanList) :-
   cleanEquals(List, Swaps),
   rotateAll(Swaps,SwapsRotated),
   addKeys(SwapsRotated,SwapsWithKeys),
   keysort(SwapsWithKeys,SwapsSorted),
   removeKeys(SwapsSorted,CleanList).
   
rotateAll([],[]).
rotateAll([Head|Tail],[HeadRotated|TailRotated]) :-
	rotateHighestFirst(Head,HeadRotated),
	rotateAll(Tail,TailRotated).

addKeys([],[]).
addKeys([Head|Swaps],[Key-Head|SwapsWithKeys]) :-
	listOfHights(Head,Hights),
	amountOfPasses(Head,Number),
	Key = [Number,Hights],
	addKeys(Swaps,SwapsWithKeys).

removeKeys([],[]).
removeKeys([_Key-Head|SwapsWithKeys],[Head|Swaps]) :-
	removeKeys(SwapsWithKeys,Swaps).

convertP([], [], _, _).
convertP([p(FirstThrow,IndexDown,Origen) | RestThrows ], [  FirstThrowP| RestThrowsP], Length, Persons) :- 
	pStyle(Length, Origen, Style),
	float_to_shortpass(FirstThrow,FirstThrowShort),
	((
		Persons > 2,
		IndexUp is Persons - IndexDown,
		sformat(FirstThrowP, "<span class='~w'>~wp<sub>~w</sub></span>", [Style,FirstThrowShort,IndexUp])
	);
	(
		Persons = 2,
		sformat(FirstThrowP, "<span class='~w'>~wp</span>", [Style,FirstThrow])
	)),
    convertP(RestThrows, RestThrowsP, Length, Persons).
convertP([ FirstThrow | RestThrows ], [ FirstThrow | RestThrowsP], Length, Persons) :-
    number(FirstThrow),
	%%atom_concat(FirstThrow, '', FirstThrowP),
    convertP(RestThrows, RestThrowsP, Length, Persons).
convertP([ FirstThrow | RestThrows ], [ FirstThrowP | RestThrowsP], Length, Persons) :-
    is_list(FirstThrow),
	convertP(FirstThrow,FirstThrowP, Length, Persons),
    convertP(RestThrows, RestThrowsP, Length, Persons).

convertMultiplex([],[]).
convertMultiplex([Multiplex | Rest], [MultiplexNew | RestNew]) :-
    is_list(Multiplex), !,
 	concat_atom(Multiplex, ',', MultiplexTemp),
 	atom_concat('[',MultiplexTemp,MultiplexTemp2),
 	atom_concat(MultiplexTemp2, ']', MultiplexNew),
 	convertMultiplex(Rest, RestNew).
convertMultiplex([Throw | Rest], [Throw | RestNew]) :-
	convertMultiplex(Rest, RestNew).

writeSwap(Throws) :-
   concat_atom(Throws, ' ', Swap),
   format('~w<br>\n', [Swap]),!.

writePassingSwap(Throws, Persons) :-
	length(Throws,Length),
    convertP(Throws, ThrowsP, Length, Persons),
	convertMultiplex(ThrowsP,ThrowsPM),
    writeSwap(ThrowsPM).
