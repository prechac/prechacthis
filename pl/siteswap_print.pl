
allSiteswaps(Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, MaxNumberOfResults, BackURL) :-
   catch(
   (
      get_time(Start),
      findAtMostNUnique(Throws, 
         siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React),
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
		format(string(FirstThrowP), "<span class='~w'>~wp<sub>~w</sub></span>", [Style,FirstThrowShort,Index])
	);
	(
		Persons = 2,
		format(string(FirstThrowP), "<span class='~w'>~wp</span>", [Style,FirstThrowShort])
	)),
    convertP(RestThrows, RestThrowsP, Length, Persons).
convertP([ p(FirstThrow, 0, _Origen)  | RestThrows ], [ FirstThrowP | RestThrowsP], Length, Persons) :-
    number(FirstThrow),
	format(string(FirstThrowP), "~w", [FirstThrow]),
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

convertMagic(Pattern, [], Pattern) :- !.
convertMagic(Pattern, MagicPositions, MagicPattern) :-
	convertMagicThrows(Pattern, MagicPositions, MagicPattern),
	fillIn(Pattern, MagicPattern, 0, MagicPositions).

convertMagicThrows(_, [], _) :- !.
convertMagicThrows(Pattern, [Pos|MagicPositions], MagicPattern) :- 
	nth0(Pos, Pattern, Throw),
	format(string(MagicThrow), "<span class='magic' title='it&#39;s&nbsp;magic'>~s</span>", [Throw]),
	nth0(Pos, MagicPattern, MagicThrow),
	convertMagicThrows(Pattern, MagicPositions, MagicPattern).


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
   pattern_to_string(ThrowsShort, URLPattern),
   format("<a href='./info.php?pattern=~s&persons=~w&back=~w'>~w</a><br>\n", [URLPattern,Persons,BackURL,Swap]),!.

writePassingSwap(Throws, Persons, BackURL) :-
	length(Throws,Length),
    convertP(Throws, ThrowsP, Length, Persons),
	convertMultiplex(ThrowsP,ThrowsPM),
    writeSwap(ThrowsPM, Throws, Persons, BackURL).

writeCompletedSiteswap(Pattern, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React, BackURL) :-
   siteswap(Throws, Persons, Objects, Length, Max, NumberOfMultiplexes, PassesMin, PassesMax, Contain, DontContain, ClubDoes, React),
   rotateHighestFirst(Throws, Pattern),
   writePassingSwap(Pattern, Persons, BackURL).


pattern_to_string(Pattern, PatternStr) :-
	format(atom(TempStr), "~w", [Pattern]),
	string_to_list(TempStr, TempLst),
	remove_whitespace(TempLst, PatternStr).


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

