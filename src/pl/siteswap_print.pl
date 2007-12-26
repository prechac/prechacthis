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


cleanListOfSiteswaps(List,CleanList) :-
   cleanEquals(List, Swaps),
   rotateAll(Swaps,SwapsRotated),
   addKeys(SwapsRotated,SwapsWithKeys),
   keysort(SwapsWithKeys,SwapsSorted),
   removeKeys(SwapsSorted,CleanList).


cleanEquals([],[]).
cleanEquals([Head|Tail], CleanBag) :-
	containsEqual(Tail, Head),!,
	cleanEquals(Tail,CleanBag).
cleanEquals([Head|Tail], [Head|CleanBag]) :-
	cleanEquals(Tail, CleanBag).

containsEqual([Head|_Tail],Siteswap) :-
	areEqual(Siteswap,Head),!.
containsEqual([_Head|Tail],Siteswap) :-
	containsEqual(Tail,Siteswap).

areEqual(P1,P2) :-
	permutateMultiplexes(P1, PTemp),
	rotate(PTemp,P2).
	
   
rotateAll([],[]).
rotateAll([Head|Tail],[HeadRotated|TailRotated]) :-
	rotateHighestFirst(Head,HeadRotated),
	rotateAll(Tail,TailRotated).

addKeys([],[]).
addKeys([Head|Swaps],[Key-Head|SwapsWithKeys]) :-
	listOfHeights(Head,Heights),
	amountOfPasses(Head,Number),
	Key = [Number,Heights],
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
		sformat(FirstThrowP, "<span class='~w'>~wp</span>", [Style,FirstThrowShort])
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

%% single throw:
convertP(p(Throw, IndexDown, Origen), ThrowP, Length, Persons) :-
	convertP([p(Throw, IndexDown, Origen)], [ThrowP], Length, Persons).

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

writeSwap(ThrowsPM, Throws, Persons) :-
   concat_atom(ThrowsPM, ' ', Swap),
   float_to_shortpass(Throws,ThrowsShort),
   format("<a href='./info.php?pattern=~w&persons=~w'>~w</a><br>\n", [ThrowsShort,Persons,Swap]),!.

writePassingSwap(Throws, Persons) :-
	length(Throws,Length),
    convertP(Throws, ThrowsP, Length, Persons),
	convertMultiplex(ThrowsP,ThrowsPM),
    writeSwap(ThrowsPM, Throws, Persons).
